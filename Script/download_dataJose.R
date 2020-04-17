
library(readr)
library(nplr)
library(dplyr)
library(DT)
library(utils)
library(httr)
library(rvest)
library(readxl)
library(tidyverse)


#Obtaining the data

urlfile="https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/nacional_covid19.csv"
nacional<-read_csv(url(urlfile))
urlfile="https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/nacional_covid19_rango_edad.csv"
#nacional_edad<-read_csv(url(urlfile))
urlfile="https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_casos.csv"
ccaa_casos<-read_csv(url(urlfile))
urlfile="https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_fallecidos.csv"
ccaa_fallecidos<-read_csv(url(urlfile))
urlfile="https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_hospitalizados.csv"
ccaa_hospitalizados<-read_csv(url(urlfile))
urlfile="https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_uci.csv"
ccaa_uci<-read_csv(url(urlfile))
urlfile="https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_altas.csv"
ccaa_altas<-read_csv(url(urlfile))


urlfile="https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv"
italia_regiones=read_csv(url(urlfile))
urlfile="https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv"
italia=read_csv(url(urlfile))
urlfile="https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_camas_uci_2017.csv"
#camas_uci=read_csv(url(urlfile))


urlfile="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
hopkings_confirmed<-read_csv(url(urlfile))
urlfile="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
hopkings_deaths<-read_csv(url(urlfile))
urlfile="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
hopkings_recovered<-read_csv(url(urlfile))


urlfile="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
USA_confirmed<-read_csv(url(urlfile))
urlfile="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
USA_deaths<-read_csv(url(urlfile))
#urlfile="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_US.csv"
#USA_recovered<-read_csv(url(urlfile))




#download the dataset from the ECDC website to a local temporary file
GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))

#read the Dataset sheet into “R”. The dataset will be called "data".
ECDC <- read.csv(tf)


urlfile="https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv"
belgica_casos<-read_csv(url(urlfile, encoding="UTF-8"))

urlfile="https://epistat.sciensano.be/Data/COVID19BE_MORT.csv"
belgica_fallecidos<-read_csv(url(urlfile))

urlfile="https://epistat.sciensano.be/Data/COVID19BE_HOSP.csv"
belgica_hospitalizados<-read_csv(url(urlfile))

urlfile="https://epistat.sciensano.be/Data/COVID19BE_tests.csv"
belgica_tests<-read_csv(url(urlfile))


tryCatch({
spaini=list()
spaini$regiones=c("Spain",ccaa_casos$CCAA[1:19])
spaini$variables=colnames(nacional)[c(4,2,3,5,6)]
spaini$variablesen=c("Deceased","Confirmed cases","Hospital discharges","Intensive Care patients","Total hospitalised patients")
})
tryCatch({
italyi=list()
italyi$regiones=c("Italy",unique(italia_regiones$denominazione_regione))
italyi$variables=colnames(italia_regiones)[c(15,16,14,7,8,9,10,17)]
italyi$variablesen=c("Deceased","Confirmed cases","Recovered","Hospitalised patients with symptoms","Intensive care", "Total hospitalized patients", "Home isolation","Tests performed")
})

tryCatch({
spain=nacional
spain=as.data.frame(spain)
matrixna=matrix(NA,nrow=dim(nacional)[1]*19,ncol=dim(nacional)[2])
colnames(matrixna)=names(spain)
spain=rbind(spain,matrixna)
spain=cbind(spain,rep(c("Spain",ccaa_casos$CCAA[1:19]),dim(nacional)[1],dim(nacional)[1]*20,each=dim(nacional)[1]))
names(spain)[dim(spain)[2]]="region"
spain[,1]=rep(spain[1:dim(nacional)[1],1],20)
names(spain)[1]="date"

for(k in 1:5){
  if(k==1){ccaat=ccaa_casos}
  if(k==2){ccaat=ccaa_altas}
  if(k==3){ccaat=ccaa_fallecidos}
  if(k==4){ccaat=ccaa_uci}
  if(k==5){ccaat=ccaa_hospitalizados}
  ccaat=as.data.frame(ccaat)
  ccaat=ccaat[1:19,]
  nadded=dim(nacional)[1]-(dim(ccaat)[2]-2)
  ccaat=ccaat[,3:dim(ccaat)[2]]
  matrixna=matrix(NA,nrow=dim(ccaat),ncol=nadded)
  ccaat=as.double(t(cbind(matrixna,ccaat)))
  spain[(dim(nacional)[1]+1):dim(spain)[1],k+1]=ccaat
}
})

tryCatch({
italy=cbind(italia[,1:2],rep(NA,dim(italia)[1]),rep("Italy",dim(italia)[1]),rep(NA,dim(italia)[1]),rep(NA,dim(italia)[1]),italia[,3:15])
italy=as.data.frame(italy)
names(italy)=names(italia_regiones)
italy=rbind(italy,italia_regiones)
names(italy)[4]="regiones"
names(italy)[1]="date"
})

tryCatch({
USA=cbind(as.double(unlist(t(USA_deaths[,13:dim(USA_deaths)[2]]))),as.double(unlist(t(USA_confirmed[,12:dim(USA_confirmed)[2]]))))
USA=as.data.frame(USA)
names(USA)=c("Deaths","Confirmed")
USAp=USA_deaths$"Combined_Key"
USAp[is.na(USAp)]=""
USAr=USAp
USAr=rep(USAr,dim(USA_deaths)[2]-12,dim(USA_deaths)[1]*(dim(USA_deaths)[2]-12),each=dim(USA_deaths)[2]-12)
USA=cbind(USA,as.character(USAr))
names(USA)[3]="regiones"
USAd=names(USA_deaths)[13:dim(USA_deaths)[2]]
USAd=rep(USAd,dim(USA_deaths)[1])
USA=cbind(as.Date(USAd, "%m/%d/%y"),USA)
names(USA)[1]="date"
#USAtot=cbind(aggregate(Deaths~date, FUN=sum,data=USA,drop=F),aggregate(Confirmed~date, FUN=sum,data=USA,drop=F)$Confirmed,rep("US",dim(USA_deaths)[2]-12))
#names(USAtot)[4]="regiones"
#names(USAtot)[3]="Confirmed"
#USA=rbind(USAtot,USA)

USAi=list()
USAi$regiones=as.character(unique(USA$regiones))
USAi$variables=c("Deaths","Confirmed")
USAi$variablesen=c("Deceased","Confirmed cases")
})



tryCatch({
  hopkings=cbind(as.double(unlist(t(hopkings_deaths[,5:dim(hopkings_deaths)[2]]))),as.double(unlist(t(hopkings_confirmed[,5:dim(hopkings_confirmed)[2]]))),as.double(unlist(t(hopkings_recovered[,5:dim(hopkings_recovered)[2]]))))
  hopkings=as.data.frame(hopkings)
  names(hopkings)=c("Deaths","Confirmed","Recovered")
  hopkingsp=hopkings_deaths$"Province/State"
  hopkingsp[is.na(hopkingsp)]=""
  hopkingsr=paste(hopkings_deaths$"Country/Region",hopkingsp)
  hopkingsr=rep(hopkingsr,dim(hopkings_deaths)[2]-4,dim(hopkings_deaths)[1]*(dim(hopkings_deaths)[2]-4),each=dim(hopkings_deaths)[2]-4)
  hopkings=cbind(hopkings,as.character(hopkingsr))
  names(hopkings)[4]="regiones"
  hopkingsd=names(hopkings_deaths)[5:dim(hopkings_deaths)[2]]
  hopkingsd=rep(hopkingsd,dim(hopkings_deaths)[1])
  hopkings=cbind(as.Date(hopkingsd, "%m/%d/%y"),hopkings)
  names(hopkings)[1]="date"
  
  hopkingsi=list()
  hopkingsi$regiones=as.character(unique(hopkings$regiones))
  hopkingsi$variables=c("Deaths","Confirmed","Recovered")
  hopkingsi$variablesen=c("Deceased","Confirmed cases","Recovered")
})

tryCatch({
ECDC=ECDC[(dim(ECDC)[1]):1,]
ECDC=as.data.frame(ECDC)
ECDC[,1]=as.Date(ECDC[,1], "%d/%m/%Y")
names(ECDC)[1]="date"
names(ECDC)[c(6,5,7)]=c("deaths","cases","regiones")

for(regused in unique(ECDC$regiones)){
ECDC[ECDC$regiones==regused,5]=cumsum(ECDC[ECDC$regiones==regused,5])
ECDC[ECDC$regiones==regused,6]=cumsum(ECDC[ECDC$regiones==regused,6])
}  

ECDCi=list()
ECDCi$regiones=as.character(sort(unique(ECDC$regiones)))
ECDCi$variables=c("deaths","cases")
ECDCi$variablesen=c("Deceased","Confirmed cases")
})





tryCatch({

belgica_casos2=belgica_casos
belgica_casos=as.data.frame(belgica_casos)
#belgica_casos$PROVINCE[is.na(belgica_casos$PROVINCE)]="NA_Prov"
lbel=1
alldates=unique(belgica_casos$DATE)
alldates=as.Date(alldates[!is.na(alldates)])
alldates=seq(min(alldates),max(alldates),by=1)
for(regions in unique(belgica_casos$PROVINCE)){
  belgica_casos[(dim(belgica_casos2)[1]+lbel):(dim(belgica_casos2)[1]+length(alldates)+lbel-1),1]=alldates
  belgica_casos[(dim(belgica_casos2)[1]+lbel):(dim(belgica_casos2)[1]+length(alldates)+lbel-1),2]=rep(regions,length(alldates))
  belgica_casos[(dim(belgica_casos2)[1]+lbel):(dim(belgica_casos2)[1]+length(alldates)+lbel-1),3]=rep(belgica_casos2$REGION[which(belgica_casos2$PROVINCE==regions)[1]],length(alldates))
  belgica_casos[(dim(belgica_casos2)[1]+lbel):(dim(belgica_casos2)[1]+length(alldates)+lbel-1),6]=rep(0,length(alldates))
  
  lbel=lbel+length(alldates)
}

#tro=ave(belgica_casos[belgica_casos$PROVINCE=="WestVlaanderen",]$CASES, belgica_casos[belgica_casos$PROVINCE=="WestVlaanderen",]$DATE, FUN=cumsum)


belgicac2=aggregate(CASES~DATE, FUN=sum,data=belgica_casos,drop=F)
belgicac2$CASES=cumsum(belgicac2$CASES)


belgicac1=aggregate(CASES~DATE+REGION, FUN=sum,data=belgica_casos,drop=F)
belgicac1$CASES=ave(belgicac1$CASES, belgicac1$REGION, FUN=cumsum)

belgicac=aggregate(CASES~DATE+PROVINCE, FUN=sum,data=belgica_casos,drop=F)
belgicac$CASES=ave(belgicac$CASES, belgicac$PROVINCE, FUN=cumsum)

belgicac2=aggregate(CASES~DATE, FUN=sum,data=belgica_casos,drop=F)
belgicac2$CASES=cumsum(belgicac2$CASES)






belgica_fallecidos2=belgica_fallecidos
belgica_fallecidos=as.data.frame(belgica_fallecidos)
#belgica_fallecidos$PROVINCE[is.na(belgica_fallecidos$PROVINCE)]="NA_Prov"
lbel=1

for(regions in unique(belgica_fallecidos$REGION)){
  belgica_fallecidos[(dim(belgica_fallecidos2)[1]+lbel):(dim(belgica_fallecidos2)[1]+length(alldates)+lbel-1),1]=alldates
  belgica_fallecidos[(dim(belgica_fallecidos2)[1]+lbel):(dim(belgica_fallecidos2)[1]+length(alldates)+lbel-1),2]=rep(regions,length(alldates))
  #belgica_fallecidos[(dim(belgica_fallecidos2)[1]+lbel):(dim(belgica_fallecidos2)[1]+length(alldates)+lbel-1),5]=rep(belgica_fallecidos2$REGION[which(belgica_fallecidos2$PROVINCE==regions)[1]],length(alldates))
  belgica_fallecidos[(dim(belgica_fallecidos2)[1]+lbel):(dim(belgica_fallecidos2)[1]+length(alldates)+lbel-1),5]=rep(0,length(alldates))
  
  lbel=lbel+length(alldates)
}


belgicaf2=aggregate(DEATHS~DATE, FUN=sum,data=belgica_fallecidos,drop=F)
belgicaf2$DEATHS=cumsum(belgicaf2$DEATHS)


belgicaf1=aggregate(DEATHS~DATE+REGION, FUN=sum,data=belgica_fallecidos,drop=F)
belgicaf1$DEATHS=ave(belgicaf1$DEATHS, belgicaf1$REGION, FUN=cumsum)






belgicat2=as.data.frame(belgica_tests)









belgica_hospitalizados2=belgica_hospitalizados
belgica_hospitalizados=as.data.frame(belgica_hospitalizados)
#belgica_hospitalizados$PROVINCE[is.na(belgica_hospitalizados$PROVINCE)]="NA_Prov"
lbel=1

for(regions in unique(belgica_hospitalizados$PROVINCE)){
  belgica_hospitalizados[(dim(belgica_hospitalizados2)[1]+lbel):(dim(belgica_hospitalizados2)[1]+length(alldates)+lbel-1),1]=alldates
  belgica_hospitalizados[(dim(belgica_hospitalizados2)[1]+lbel):(dim(belgica_hospitalizados2)[1]+length(alldates)+lbel-1),2]=rep(regions,length(alldates))
  belgica_hospitalizados[(dim(belgica_hospitalizados2)[1]+lbel):(dim(belgica_hospitalizados2)[1]+length(alldates)+lbel-1),3]=rep(belgica_hospitalizados2$REGION[which(belgica_hospitalizados2$PROVINCE==regions)[1]],length(alldates))
  belgica_hospitalizados[(dim(belgica_hospitalizados2)[1]+lbel):(dim(belgica_hospitalizados2)[1]+length(alldates)+lbel-1),9]=rep(0,length(alldates))
  belgica_hospitalizados[(dim(belgica_hospitalizados2)[1]+lbel):(dim(belgica_hospitalizados2)[1]+length(alldates)+lbel-1),10]=rep(0,length(alldates))
  
  lbel=lbel+length(alldates)
}

#tro=ave(belgica_hospitalizados[belgica_hospitalizados$PROVINCE=="WestVlaanderen",]$NEW_IN, belgica_hospitalizados[belgica_hospitalizados$PROVINCE=="WestVlaanderen",]$DATE, FUN=cumsum)


belgicah2=aggregate(NEW_IN~DATE, FUN=sum,data=belgica_hospitalizados,drop=F)
belgicah2$NEW_IN=cumsum(belgicah2$NEW_IN)

belgicah1=aggregate(NEW_IN~DATE+REGION, FUN=sum,data=belgica_hospitalizados,drop=F)
belgicah1$NEW_IN=ave(belgicah1$NEW_IN, belgicah1$REGION, FUN=cumsum)

belgicah=aggregate(NEW_IN~DATE+PROVINCE, FUN=sum,data=belgica_hospitalizados,drop=F)
belgicah$NEW_IN=ave(belgicah$NEW_IN, belgicah$PROVINCE, FUN=cumsum)








belgicar2=aggregate(NEW_OUT~DATE, FUN=sum,data=belgica_hospitalizados,drop=F)
belgicar2$NEW_OUT=cumsum(belgicar2$NEW_OUT)

belgicar1=aggregate(NEW_OUT~DATE+REGION, FUN=sum,data=belgica_hospitalizados,drop=F)
belgicar1$NEW_OUT=ave(belgicar1$NEW_OUT, belgicar1$REGION, FUN=cumsum)

belgicar=aggregate(NEW_OUT~DATE+PROVINCE, FUN=sum,data=belgica_hospitalizados,drop=F)
belgicar$NEW_OUT=ave(belgicar$NEW_OUT, belgicah$PROVINCE, FUN=cumsum)




belgicacount=data.frame(date=belgicac2$DATE,region=rep("Belgium",length(belgicac2$DATE)),cases=belgicac2$CASES,deaths=belgicaf2$DEATHS,hospital_intakes=belgicah2$NEW_IN,hospital_discharges_alive=belgicar2$NEW_OUT,tests=c(rep(0,length(alldates)-length(belgicat2$TESTS)),cumsum(belgicat2$TESTS)))

belgicareg=data.frame(date=belgicac1$DATE,region=belgicac1$REGION,cases=belgicac1$CASES,deaths=belgicaf1$DEATHS,hospital_intakes=belgicah1$NEW_IN,hospital_discharges_alive=belgicar1$NEW_OUT,tests=rep(NA,length(belgicac1$CASES)))

belgicaprov=data.frame(date=belgicac$DATE,region=belgicac$PROVINCE,cases=belgicac$CASES,deaths=rep(NA,length(belgicac$CASES)),hospital_intakes=belgicah$NEW_IN,hospital_discharges_alive=belgicar$NEW_OUT,tests=rep(NA,length(belgicac$CASES)))

levels(belgicaprov$region)[3]="Brussels_province"

belgica=rbind(belgicacount,belgicareg,belgicaprov)

levels(belgica$region)[9]="Liege"

belgicai=list()
belgicai$regiones=as.character(sort(unique(belgica$region)))
belgicai$variables=names(belgica)[c(4,3,6,5,7)]
belgicai$variablesen=c("Deceased","Confirmed cases","Hospital discharges","Total hospitalised patients","Total tests")

})









tryCatch({
portugaltot=read.csv2("COVIDvariables_daily_NACIONAL_Portugal.csv")

#Nº de infectados, óbitos,Casos suspechos,casos não confirmados,Hospitalizados, Casos recuperados
#óbitos(regiões), recuperados(regiões), (regiões), 
portugal1=matrix(as.double(t(portugaltot[c(2,3,17,12,15,16)-1,3:(dim(portugaltot)[2]-1)])),ncol=6)
colnames(portugal1)=c("casos confirmados","óbitos", "Casos recuperados","casos suspeitos","casos não confirmados","hospitalizados")
date1=as.Date("2020-03-03")
date2=as.Date("2020-12-31")
dates=seq(date1,date2,by=1)[1:(dim(portugal1)[1])]

portugal=data.frame(date=dates,regiones="Portugal",portugal1)
portugal3=matrix(NA,nrow=dim(portugal)[1],ncol=6)
colnames(portugal3)=c("casos confirmados","óbitos", "Casos recuperados","casos suspeitos","casos não confirmados","hospitalizados")
portugal2=data.frame(date=rep(dates,8),regiones=rep(as.character(portugaltot[3:10,2]),each=dim(portugal1)[1]),portugal3)

for(j in 1:8){
  portugal2[(dim(portugal)[1]*(j-1)+1):(j*dim(portugal)[1]),3:5]=matrix(as.double(t(portugaltot[c(86,18,4)+j-2,3:(dim(portugaltot)[2]-1)])),ncol=3)
}


portugal=rbind(portugal,portugal2)


portugali=list()
portugali$regiones=as.character(sort(unique(portugal$regiones)))
portugali$variables=names(portugal)[c(2,1,3,6,4,5)+2]
portugali$variablesen=c("Deceased","Confirmed cases","Recovered","Total hospitalised patients","Suspicious cases","Unconfirmed cases")


})



#From tidycovid19

download_acaps_npi_data=function (silent = FALSE, cached = FALSE) {
  if (length(silent) > 1 || !is.logical(silent)) 
    stop("'silent' needs to be a single logical value")
  if (length(cached) > 1 || !is.logical(cached)) 
    stop("'silent' needs to be a single logical value")
  if (cached) {
    if (!silent) 
      message("Downloading cached version of ACAPS NPI data...", 
              appendLF = FALSE)
    df <- readRDS(gzcon(url("https://raw.githubusercontent.com/joachim-gassen/tidycovid19/master/cached_data/acaps_npi.RDS")))
    if (!silent) 
      message(sprintf("done. Timestamp is %s", df$timestamp[1]))
    return(df)
  }
  if (!silent) 
    message("Start downloading ACAPS NPI data\n")
  url <- "https://data.humdata.org/dataset/acaps-covid19-government-measures-dataset"
  selector_path <- paste0("#data-resources-0 > div > ul > li > ", 
                          "div.hdx-btn-group.hdx-btn-group-fixed > ", "a.btn.btn-empty.btn-empty-blue.hdx-btn.resource-url-analytics.ga-download")
  dta_url <- xml2::read_html(url) %>% rvest::html_node(css = selector_path) %>% 
    rvest::html_attr("href")
  tmp_file <- tempfile(".xlsx")
  utils::download.file(paste0("https://data.humdata.org", 
                              dta_url), tmp_file, quiet = silent, mode = "wb")
  raw_dta <- readxl::read_excel(tmp_file, sheet = "Database")
  df <- raw_dta
  names(df) <- tolower(names(df))
  names(df)[16] <- "alternative_source"
  df$category[df$category == "Movement Restriction"] <- "Movement restrictions"
  df$category[df$category == "Movement Restrictions"] <- "Movement restrictions"
  df$category[df$category == "Social and Economic Measures"] <- "Social and economic measures"
  df$category[df$category == "Social Distancing"] <- "Social distancing"
  df <- df %>% dplyr::select(-.data$pcode) %>% dplyr::filter(!is.na(.data$date_implemented), 
                                                             !is.na(.data$category)) %>% dplyr::rename(iso3c = .data$iso) %>% 
    dplyr::mutate(timestamp = Sys.time())
  if (!silent) 
    message("Done downloading ACAPS NPI data\n")
  df
}


df=download_acaps_npi_data()






firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  return(x)
}

funnplrmod=function(datos){
  datos2=c(0,datos)
  pdatos=which(datos2==0)
  pdatos=pdatos[length(pdatos)]
  pdatos=max(1,pdatos-4)
  return(nplr(x=pdatos:(length(datos)), y=datos[pdatos:(length(datos))], useLog=FALSE))
}

funsep<- function(vec, val, k){
  vec[order(abs(vec-val))][seq_len(k)]
}