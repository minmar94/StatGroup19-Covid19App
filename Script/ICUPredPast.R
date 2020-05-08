# Packages
require(tidyverse)
require(magrittr)
require(lubridate)
require(DT)
require(stringi)
require(MASS)
require(rmutil)
require(nplr)
require(tscount)
require(foreach)
require(doSNOW)
require(lme4)

rm(list=ls())

# Read and prepare regional data
read_regional <- function(path){
  
  dati_reg <- read_csv(path) %>% 
    dplyr::select(-note_it, -note_en) %>% 
    # Creo la regione Trentino che appare sottoforma di province autonome
    mutate(denominazione_regione = ifelse(denominazione_regione %in% c("P.A. Bolzano", "P.A. Trento"), "Trentino-Alto Adige", denominazione_regione),
           data = date(data)) %>% 
    gather(Key, Value, ricoverati_con_sintomi:tamponi) %>% 
    group_by(Key, data, denominazione_regione) %>% 
    dplyr::summarise(Value = sum(Value)) %>% 
    ungroup() 
  
  dati_reg$Key <- factor(dati_reg$Key, levels = unique(dati_reg$Key), 
                         labels = c("Deceased", "Discharged healed", "Home isolation", "New positives", "Hospitalized with symptoms", 
                                    "Swabs", "Intensive care", "Total cases", "Currently hospitalized", "Currently positives", 
                                    "Variation currently positives"))
  
  return(dati_reg)
}

# Prepare data per modello terapie intensive a parte
dataprep_terapie <- function(dftoprep, resdata){
  
  ag <- dftoprep %>% 
    mutate(denominazione_regione = ifelse(denominazione_regione == "Trentino-Alto Adige", "TrentinoAltoAdige", 
                                          ifelse(denominazione_regione == "Friuli Venezia Giulia", "Friuli V. G.", denominazione_regione))) %>% 
    arrange(denominazione_regione, data)
  
  # Aggiusto le etichette
  #levels(fa2)[6] <- "Friuli V. G."
  resdata[10,1] <- "Valle d'Aosta"
  resdata[48,1] <- "Emilia-Romagna"
  
  
  da <- ag %>% dplyr::select(data, denominazione_regione, `Intensive care`) %>% 
    left_join(resdata, by = c("denominazione_regione" = "Territorio")) %>% 
    mutate(data = as.numeric(unclass(factor(data))), denominazione_regione = factor(denominazione_regione)) %>% 
    rename(ti = data, region = denominazione_regione, icu = `Intensive care`, residents = totale) 
  
  mx <- max(da$ti)-15
  #mx_orig <- max(ti_orig)-15
  da <- da[da$ti>mx,]
  da$ti <- da$ti-mx
  #da$ti_orig <- ti_orig-mx
  
  da.pred <- da[which(da$ti==max(da$ti)),]
  da.pred$ti <- max(da$ti)+1
  da <- da[order(da$region,da$ti),]
  da.pred <- da.pred[order(da.pred$region),]
  
  #da.pred$capienza=c(115,49,107,506,539,127,557,186,1200,154,31,560,289,123,392,745,115,70,12,600)
  # Aggiornamento capienza
  da.pred$capienza <- c(115,49,107,(506+80),(539+90),127,(557+118),186,(1200+208),(154+39),31,560,289,123,392,(394+70),(115+42),70,45,(600+338))
  da$capienza <- rep(da.pred$capienza,each=max(da$ti))
  
  
  return(list(da = da, dapred = da.pred))
}

fup <- function(dat){
  y <- dat$icu
  x <- cbind(dat$ti,dat$ti^2/100,dat$ti^3/250)
  tsw3 <- tsglm(y,xreg=x[,1:3],link="log",dist="poisson")
  tsw2 <- tsglm(y,xreg=x[,1:2],link="log",dist="poisson")
  tsw1 <- tsglm(y,xreg=x[,1],link="log",dist="poisson")
  tsw <- tsglm(y,link="log",dist="poisson")
  newx <- NULL
  if(BIC(tsw)>BIC(tsw1)){
    tsw <- tsw1; newx=data.frame(ti=max(x[,1])+1)
  }
  if(BIC(tsw)>BIC(tsw2)){
    tsw <- tsw2
    newx <- data.frame(ti=max(x[,1])+1,ti2=(max(x[,1])+1)^2/100)
  }
  if(BIC(tsw)>BIC(tsw3)){
    tsw <- tsw3
    mx <- max(x[,1])+1
    newx <- data.frame(ti=mx,ti2=mx^2/100,ti3=mx^3/250)
  }
  
  pr <- predict(tsw, newxreg=newx,level=1-0.01)
  
  outvec <- c(pr$pred,pr$interval)
  names(outvec) <- c("est", "lb", "ub")
  return(outvec)
}

modello_terapia_intensiva <- function(dat = da, dattopred = da.pred){
  
  fit2 <- glmer(icu~ti+I(ti^2/100)+offset(log(residents))+((1+ti)|region)+((0+I(ti^2/100))|region),data=dat,family=poisson)
  pr2 <- exp(predict(fit2,dattopred))
  
  ba <- dat %>% group_split(region) %>% sapply(FUN = fup, simplify = T) %>% t
  
  # optW (unique)
  
  d2 <- dat[dat$ti<max(dat$ti),]
  fit.loss <- glmer(icu~ti+I(ti^2/100)+offset(log(residents))+((1+ti)|region)+((0+I(ti^2/100))|region),data=d2,family=poisson)
  pr.loss <- exp(predict(fit.loss,d2))
  ba.loss <- d2 %>% group_split(region) %>% sapply(FUN = fup, simplify = T) %>% t
  
  do <- function(x,b,l,da){
    w <- exp(x)/(1+exp(x))
    abs(b*w+l*(1-w)-da$icu)^2
  }
  
  optW <- rep(NA,nrow(dattopred))
  for(j in 1:length(optW)) {
    op <- optimize(function(x) do(x ,ba.loss[,1],pr.loss,d2)[j],c(-15,15))
    optW[j] <- exp(op$min)/(1+exp(op$min))
  }
  
  jnk <- list(preds=round(pr2*(1-optW)+ba[,1]*optW),tsci=ba[,-1],optW=optW)
  
  pred <- data.frame(region=levels(dat$region),prediction=jnk$preds)
  
  cl <- makeCluster(3, type = "SOCK")
  registerDoSNOW(cl)
  
  res2 <- matrix(NA, nrow = 500, ncol = nrow(dattopred))
  res2 <- foreach(j = 1:500, .packages = "lme4", .combine = "rbind")  %dopar% {
    ws <- sample(nrow(dat),nrow(dat),replace=TRUE)
    daws <- dat[ws,]
    fit2 <- glmer(icu~ti+I(ti^2/100)+offset(log(residents))+((1+ti)|region)+((0+I(ti^2/100))|region),data=daws,family=poisson)
    pr2 <- exp(predict(fit2,dattopred))
    return(pr2)
  }
  
  stopCluster(cl)
  
  qu12 <- apply(res2, 2, quantile, probs = c(0.01/2, 1-0.01/2), na.rm=TRUE)
  pred$prLow.Bonf <- apply(cbind(round(qu12[1,]),jnk$tsci[,1]),1,min)
  pred$prUp.Bonf <- apply(cbind(round(qu12[2,]),jnk$tsci[,2]),1,max)
  pred$prUp.Bonf <- pmin(pred$prUp.Bonf,dattopred$capienza)
  pr.oggi <- pred
  pr.oggi$capienza <- dattopred$capienza 
  
  colnames(pr.oggi) <- c("Region", "Prediction", "Lower bound", "Upper bound", "Capacity")
  
  return(pr.oggi)
  
}

load("C:/Users/marco/OneDrive/Desktop/Marco/Universita/StatGroup19Eng/Data/ResICU.RData")

# Read regional data up to today
dati_reg <- read_regional(path = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")

# Leggo dati residenti
residents[,1] <- as.character(residents[,1])

df <- dati_reg %>% spread(Key, Value)

# Run Model daily starting 16 after the 24th of February ------------------
dates <- unique(dati_reg$data)
est_dates <- dates[dates > min(dates)+15]

outmod_terapie_tab <- list()
cnt <- 1
for(dd in est_dates){
  
  print(est_dates[cnt])
  df1 <- df %>% filter(data <= dd)
  
  icu_data <- dataprep_terapie(dftoprep = df1, resdata = residents)
  outmod_terapie_tab[[cnt]] <- modello_terapia_intensiva(dat = icu_data$da, dattopred = icu_data$dapred) %>% 
    as_tibble() %>% 
    mutate(DataPred = dd + 1)
  
  cnt = cnt + 1
  
}

outmod_terapie_tab %<>% 
  map2_dfr(as.list(est_dates), function(x, y) x %<>% mutate(DataPred = y + 1))

save(outmod_terapie_tab, file = "C:/Users/marco/OneDrive/Desktop/Marco/Universita/StatGroup19Eng/Data/PastICUPred.RData")

