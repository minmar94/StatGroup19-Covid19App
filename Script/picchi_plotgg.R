library(nplr)

rm(list=ls())
load("Data/DatiRegione1Apr_recode.RData")

source("Script/_growthGLM.r")

ag=dati
icu=ag$terapia_intensiva
fa=factor(ag[,1])
fa2=ag[,2]
ti=unclass(fa)

#library(flexmix)
library(tscount)

library(foreach)
library(doSNOW)

levels(fa2)[6]="Friuli V. G."

residents=read.table("Data/residenti2019.csv",header=TRUE,sep=",")
residents[,1]=as.character(residents[,1])
residents[10,1]="Valle dAosta"

ma <- match(residents[,1],levels(fa2))
residents <- residents[which(!is.na(ma)),]
ma <- match(fa2,residents[,1])

fa2 <- as.character(fa2)

da <- data.frame(dati[,-c(1:2)],ti=as.numeric(ti),region=fa2,residents=residents[ma,2])

da$region <- factor(as.character(da$region))

do=function(reg,wh=1,fam="nb",useLog=T) {

dat=da[da$region==reg,]

    pc=dat[,wh]
    ti=dat$ti
    
    if(any(pc==0)) {
        w=min(which(pc!=0))
        pc=pc[-c(1:w)]
        ti=ti[-c(1:w)]
        ti=ti-min(ti)+1}
mti=max(ti)        
    
np=growthGLM(pc,ti,timax=50,family=fam,nstart=1000,useLog=useLog)

ps=np$pars
y=np$linPred
x=1:50

    require(ggplot2)
require(gridExtra)
cc1<-data.frame(x1=c(x[2:mti],rep(NA,length(x[-c(1:mti)]))),pc=c(diff(pc),rep(NA,length(x[-c(1:mti)]))), x=x[-1],y=diff(y))
cc<-data.frame(x1=c(x[1:mti],rep(NA,length(x[-c(1:mti)]))),pc=c(pc,rep(NA,length(x[-c(1:mti)]))), x=x,y=y)
grid.arrange(

ggplot(cc)+geom_point(aes(x=x1,y=pc))+geom_line(aes(x=x,y=y),col="red")+ylab("Cumulative Cases")+xlab("Days")+labs(title=paste(names(dat)[wh],reg,sep=" ")),

ggplot(cc1)+geom_point(aes(x=x1,y=pc))+geom_line(aes(x=x,y=y),col="red")+ylab("New Cases")+xlab("Days")#+labs(title="Covid-19 cases in Lombardia Region")
)

    return(np) 
}

# 1, 2, 3, 5, 7, 8, 9
do(reg = "Lombardia")



# -------------------------------------------------------------------------

library(nplr)
library(tscount)
library(foreach)
library(doSNOW)

rm(list=ls())

source("Script/_growthGLM.r")

dati <- read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv", stringsAsFactors = F) %>% arrange(denominazione_regione) %>%  
    dplyr::select(-note_it, -note_en)
ag <- dati

icu <- ag$terapia_intensiva
fa <- factor(ag[,"data"])
fa2 <- ag[,"denominazione_regione"]
fa2 <- factor(ifelse(fa2 %in% c("P.A. Bolzano", "P.A. Trento"),
       "TrentinoAltoAdige", fa2))
ti <- unclass(fa)


levels(fa2)[6] <- "Friuli V. G."
levels(fa2)[5] <- "Emilia Romagna"

residents <- read.table("Data/residenti2019.csv",header=TRUE,sep=",")
residents[,1] <- as.character(residents[,1])
residents[10,1] <- "Valle dAosta"

ma <- match(residents[,1],levels(fa2))
residents <- residents[which(!is.na(ma)),]
ma <- match(fa2,residents[,1])

fa2 <- as.character(fa2)

da <- data.frame(dati[,-c(1:6)],ti=as.numeric(ti),region=fa2,residents=residents[ma,2])

da$region <- factor(as.character(da$region))

do <- function(reg,wh=1,fam="nb",useLog=T) {
    
    dat=da[da$region==reg,]
    
    pc=dat[,wh]
    ti=dat$ti
    
    if(any(pc==0)) {
        w=min(which(pc!=0))
        pc=pc[-c(1:w)]
        ti=ti[-c(1:w)]
        ti=ti-min(ti)+1}
    mti=max(ti)        
    
    np=growthGLM(pc,ti,timax=80,family=fam,nstart=1000,useLog=useLog)
    
    ps=np$pars
    y=np$linPred
    x=1:80
    
    require(ggplot2)
    require(gridExtra)
    cc1<-data.frame(x1=c(x[2:mti],rep(NA,length(x[-c(1:mti)]))),
                    pc=c(diff(pc),rep(NA,length(x[-c(1:mti)]))), x=x[-1],y=diff(y))
    cc<-data.frame(x1=c(x[1:mti],rep(NA,length(x[-c(1:mti)]))),
                   pc=c(pc,rep(NA,length(x[-c(1:mti)]))), x=x,y=y)
    grid.arrange(
        
        ggplot(cc)+geom_point(aes(x=x1,y=pc))+
            geom_line(aes(x=x,y=y),col="red")+ylab("Cumulative Cases")+
            xlab("Days")+labs(title=paste(names(dat)[wh],reg,sep=" ")),
        
        ggplot(cc1)+geom_point(aes(x=x1,y=pc))+geom_line(aes(x=x,y=y),col="red")+
            ylab("New Cases")+xlab("Days")+labs(title=paste("Covid-19 cases in",reg))
    )
    
    return(np) 
}

# 1, 2, 3, 5, 7, 8, 9
do(reg = "Lombardia")

