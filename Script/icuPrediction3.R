rm(list=ls())
load("Data/DatiRegione1Apr_recode.RData")

ag <- dati_reg %>% spread(Key, Value) %>% arrange(denominazione_regione) %>% filter(data <= "2020-04-01")
icu <- ag$`Terapia intensiva`
fa=factor(ag$data)
fa2=factor(ag$denominazione_regione)
ti=unclass(fa)

levels(fa2)[6]="Friuli V. G."

residents=read.table("Data/residenti2019.csv",header=TRUE,sep=",")
residents[,1]=as.character(residents[,1])
residents[10,1]="Valle dAosta"
residents[48,1]="Emilia-Romagna"


ma=match(residents[,1],levels(fa2))
residents=residents[which(!is.na(ma)),]
ma=match(fa2,residents[,1])

fa2=as.character(fa2)

da=data.frame(icu=icu,ti=as.numeric(ti),region=fa2,residents=residents[ma,2])

da$region=factor(as.character(da$region))

mx=max(da$ti)-15
da=da[da$ti>mx,]
da$ti=da$ti-mx

da$icu[da$region=="Campania" & da$icu==181]=118

library(lme4)

da.pred=da[which(da$ti==max(da$ti)),]
da.pred$ti=max(da$ti)+1
da=da[order(da$region,da$ti),]
da.pred=da.pred[order(da.pred$region),]
#da.pred$capienza=c(115,49,107,506,539,127,557,186,1200,154,31,560,289,123,392,745,115,70,12,600)
# Aggiornamento capienza
da.pred$capienza=c(115,49,107,(506+80),(539+90),127,(557+118),186,(1200+208),(154+39),31,560,289,123,392,(394+70),(115+42),70,45,(600+338))
da$capienza=rep(da.pred$capienza,each=max(da$ti))

fit2 <- glmer(icu~ti+I(ti^2/100)+offset(log(residents))+((1+ti)|region)+((0+I(ti^2/100))|region),data=da,family=poisson)
pr2 <- exp(predict(fit2,da.pred))

fup <- function(dat) {
                y=dat$icu
                x=cbind(dat$ti,dat$ti^2/100,dat$ti^3/250)
                tsw3=tsglm(y,xreg=x[,1:3],link="log",dist="poisson")
                tsw2=tsglm(y,xreg=x[,1:2],link="log",dist="poisson")
                tsw1=tsglm(y,xreg=x[,1],link="log",dist="poisson")
                tsw=tsglm(y,link="log",dist="poisson")
                newx=NULL
                if(BIC(tsw)>BIC(tsw1)) {tsw=tsw1; newx=data.frame(ti=max(x[,1])+1)}
                if(BIC(tsw)>BIC(tsw2)) {tsw=tsw2;
                    newx=data.frame(ti=max(x[,1])+1,ti2=(max(x[,1])+1)^2/100)}
                if(BIC(tsw)>BIC(tsw3)) {tsw=tsw3;
                    mx=max(x[,1])+1
                    newx=data.frame(ti=mx,ti2=mx^2/100,ti3=mx^3/250)}
                                  
                pr=predict(tsw, newxreg=newx,level=1-0.01)
                
                return(c(pr$pred,pr$interval))}
    
                    ba=t(simplify2array(by(da,da$region,fup)))
             
                                        # optW (unique)
               
fit.loss <- glmer(icu~ti+I(ti^2/100)+offset(log(residents))+((1+ti)|region)+((0+I(ti^2/100))|region),data=da[da$ti<max(da$ti),],family=poisson)
pr.loss <- exp(predict(fit.loss,da[da$ti==max(da$ti),]))
ba <- t(simplify2array(by(da[da$ti<max(da$ti),],da[da$ti<max(da$ti),]$region,fup)))
                
do <- function(x,b,l,da){
        w=exp(x)/(1+exp(x))
        abs(b*w+l*(1-w)-da[da$ti==max(da$ti),]$icu)^2
}

optW=rep(NA,20)
for(j in 1:20) {
    op=optimize(function(x) do(x,ba.loss[,1],pr.loss,da)[j],c(-15,15))
    optW[j]=exp(op$min)/(1+exp(op$min))
}
        
jnk=list(preds=round(pr2*(1-optW)+ba[,1]*optW),tsci=ba[,-1],optW=optW)

pred=data.frame(region=levels(da$region),prediction=jnk$preds)

cl=makeCluster(2, type = "SOCK")
registerDoSNOW(cl)

res2 <- foreach(j = 1:500)  %dopar% {
    library(lme4)
  ws <- sample(nrow(da),nrow(da),replace=TRUE)
  daws <- da[ws,]
  fit2 <- glmer(icu~ti+I(ti^2/100)+offset(log(residents))+((1+ti)|region)+((0+I(ti^2/100))|region),data=daws,family=poisson)
  pr2 <- exp(predict(fit2,da.pred))
    return(pr2)
}

#pred$observed=c(37,2,7,22,197,19,31,73,823,110,5,186,6,0,20,107,30,15,5,156)
stopCluster(cl)
    
res2 <- matrix(unlist(res2),ncol=20,byrow=T)

#qu1=apply(res2,2,quantile,0.01/2,na.rm=TRUE)*(1-optW)
#pred$prLow.Bonf=round(qu1+jnk$tsci[,1]*optW)
#qu1=apply(res2,2,quantile,1-0.01/2,na.rm=TRUE)*(1-optW)
                                        #pred$prUp.Bonf=round(qu1+jnk$tsci[,2]*optW)
qu1 <- apply(res2,2,quantile,0.01/2,na.rm=TRUE)
pred$prLow.Bonf <- apply(cbind(round(qu1),round(jnk$tsci[,1])),1,min)
qu1 <- apply(res2,2,quantile,1-0.01/2,na.rm=TRUE)
pred$prUp.Bonf <- apply(cbind(qu1,jnk$tsci[,2]),1,max)

pred$prUp.Bonf <- pmin(pred$prUp.Bonf,da.pred$capienza)
pr.oggi <- pred
names(pr.oggi)[1] <- "Regione"
names(pr.oggi)[2] <- "Previsione"
names(pr.oggi)[3] <- "LimiteInferiore"
names(pr.oggi)[4] <- "LimiteSuperiore"
pr.oggi$capienza <- da.pred$capienza 
names(pr.oggi)[5] <- "Capienza" 
library(xtable)
print(xtable(pr.oggi,digits=0,caption="Previsioni Ricoveri in Terapia Intensiva per il 10 Aprile"),type="html",file="ICU.predictions.html",include.rownames=FALSE)

save.image(file="pred3.RData")


