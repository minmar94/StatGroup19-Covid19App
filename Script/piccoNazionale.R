rm(list=ls())
load("../RData/DatiRegione5Apr_recode.RData")

source("_growthGLM.r")

ag=dati
icu=ag$terapia_intensiva
fa=factor(ag[,1])
fa2=ag[,2]
ti=unclass(fa)
mti=max(ti)

#library(flexmix)
library(tscount)

library(foreach)
library(doSNOW)

levels(fa2)[6]="Friuli V. G."

residents=read.table("../RData/residenti2019.csv",header=TRUE,sep=",")
residents[,1]=as.character(residents[,1])
residents[10,1]="Valle dAosta"

ma=match(residents[,1],levels(fa2))
residents=residents[which(!is.na(ma)),]
ma=match(fa2,residents[,1])

fa2=as.character(fa2)

da=data.frame(deaths=dati$deceduti,pos=dati$totale_positivi,pos2=dati$totale_casi,icu=dati$terapia_intensiva,hosp=dati$totale_ospedalizzati,ti=as.numeric(ti),region=fa2,residents=residents[ma,2])

da$region=factor(as.character(da$region))

dat=aggregate(da[,1:5],list(da$ti),sum)
dat$ti=unique(da$ti)


np=growthGLM(dat$pos,dat$ti,timax=mti+15,family="Poisson",maxiter=5000,monotone=TRUE)
#library(nplr)
#npc=nplr(dat$ti,as.vector(convertToProp(dat$icu)))
#10^npc@pars[3]$x

ps=np$pars
y=np$linPred
pc=dat$`Totale positivi`
x=1:(mti+15)


jpeg("TotInfettiNazionali.jpeg",width=7,height=7,units="in",res=1000)
require(ggplot2)
require(gridExtra)
cc1<-data.frame(x1=c(x[2:mti],rep(NA,length(x[-c(1:mti)]))),pc=c(diff(pc),rep(NA,length(x[-c(1:mti)]))), x=x[-1],y=diff(y))
cc<-data.frame(x1=c(x[1:mti],rep(NA,length(x[-c(1:mti)]))),pc=c(pc,rep(NA,length(x[-c(1:mti)]))), x=x,y=y)
grid.arrange(

ggplot(cc)+geom_point(aes(x=x1,y=pc))+geom_line(aes(x=x,y=y),col="red")+ylab("Hospital occupation")+xlab("Days since 24/2")+labs(title="Positives now with Covid-19 in Italy"),

ggplot(cc1)+geom_point(aes(x=x1,y=pc))+geom_line(aes(x=x,y=y),col="red")+ylab("First difference (balance)")+xlab("Days since 24/2")#+labs(title="Covid-19 cases in Lombardia Region")
)
dev.off()
# B + (T - B)/[1 + 10^(b*(xmid - x))]^s


np=growthGLM(dat$pos2,dat$ti,timax=mti+15,family="Poisson",maxiter=5000,monotone=TRUE)
#library(nplr)
#npc=nplr(dat$ti,as.vector(convertToProp(dat$icu)))
#10^npc@pars[3]$x

ps=np$pars
y=np$linPred
pc=dat$pos2
x=1:(mti+15)


jpeg("TotCasiNazionali.jpeg",width=7,height=7,units="in",res=1000)
require(ggplot2)
require(gridExtra)
cc1<-data.frame(x1=c(x[2:mti],rep(NA,length(x[-c(1:mti)]))),pc=c(diff(pc),rep(NA,length(x[-c(1:mti)]))), x=x[-1],y=diff(y))
cc<-data.frame(x1=c(x[1:mti],rep(NA,length(x[-c(1:mti)]))),pc=c(pc,rep(NA,length(x[-c(1:mti)]))), x=x,y=y)
grid.arrange(

ggplot(cc)+geom_point(aes(x=x1,y=pc))+geom_line(aes(x=x,y=y),col="red")+ylab("Hospital occupation")+xlab("Days since 24/2")+labs(title="Positives ever with Covid-19 in Italy"),

ggplot(cc1)+geom_point(aes(x=x1,y=pc))+geom_line(aes(x=x,y=y),col="red")+ylab("First difference (balance)")+xlab("Days since 24/2")#+labs(title="Covid-19 cases in Lombardia Region")
)
dev.off()
# B + (T - B)/[1 + 1

np=growthGLM(dat$icu,dat$ti,timax=mti+15,family="nb",maxiter=5000,monotone=FALSE)
#library(nplr)
#npc=nplr(dat$ti,as.vector(convertToProp(dat$icu)))
#10^npc@pars[3]$x

ps=np$pars
y=np$linPred
pc=dat$icu
x=1:(mti+15)


jpeg("ICUNazionali.jpeg",width=7,height=7,units="in",res=1000)
require(ggplot2)
require(gridExtra)
cc1<-data.frame(x1=c(x[2:mti],rep(NA,length(x[-c(1:mti)]))),pc=c(diff(pc),rep(NA,length(x[-c(1:mti)]))), x=x[-1],y=diff(y))
cc<-data.frame(x1=c(x[1:mti],rep(NA,length(x[-c(1:mti)]))),pc=c(pc,rep(NA,length(x[-c(1:mti)]))), x=x,y=y)
grid.arrange(

ggplot(cc)+geom_point(aes(x=x1,y=pc))+geom_line(aes(x=x,y=y),col="red")+ylab("Hospital occupation")+xlab("Days since 24/2")+labs(title="ICU now with Covid-19 in Italy"),

ggplot(cc1)+geom_point(aes(x=x1,y=pc))+geom_line(aes(x=x,y=y),col="red")+ylab("First difference (balance)")+xlab("Days since 24/2")#+labs(title="Covid-19 cases in Lombardia Region")
)
dev.off()
# B + (T - B)/[1 + 1

np=growthGLM(dat$hosp,dat$ti,timax=mti+15,family="Poisson",maxiter=5000,monotone=FALSE)
#library(nplr)
#npc=nplr(dat$ti,as.vector(convertToProp(dat$icu)))
#10^npc@pars[3]$x

ps=np$pars
y=np$linPred
pc=dat$hosp
x=1:(mti+15)


jpeg("HospNazionali.jpeg",width=7,height=7,units="in",res=1000)
require(ggplot2)
require(gridExtra)
cc1<-data.frame(x1=c(x[2:mti],rep(NA,length(x[-c(1:mti)]))),pc=c(diff(pc),rep(NA,length(x[-c(1:mti)]))), x=x[-1],y=diff(y))
cc<-data.frame(x1=c(x[1:mti],rep(NA,length(x[-c(1:mti)]))),pc=c(pc,rep(NA,length(x[-c(1:mti)]))), x=x,y=y)
grid.arrange(

ggplot(cc)+geom_point(aes(x=x1,y=pc))+geom_line(aes(x=x,y=y),col="red")+ylab("Hospital occupation")+xlab("Days since 24/2")+labs(title="In-hospital now with Covid-19 in Italy"),

ggplot(cc1)+geom_point(aes(x=x1,y=pc))+geom_line(aes(x=x,y=y),col="red")+ylab("First difference (balance)")+xlab("Days since 24/2")#+labs(title="Covid-19 cases in Lombardia Region")
)
dev.off()
# B + (T - B)/[1 + 1

np=growthGLM(dat$deaths,dat$ti,timax=max(dat$ti)+15,family="nb",maxiter=5000,monotone=TRUE)
#library(nplr)
#npc=nplr(dat$ti,as.vector(convertToProp(dat$icu)))
#10^npc@pars[3]$x

ps=np$pars
y=np$linPred
pc=dat$deaths
x=1:(max(dat$ti)+15)


jpeg("DecessiNazionali.jpeg",width=7,height=7,units="in",res=1000)
require(ggplot2)
require(gridExtra)
cc1<-data.frame(x1=c(x[2:mti],rep(NA,length(x[-c(1:mti)]))),pc=c(diff(pc),rep(NA,length(x[-c(1:mti)]))), x=x[-1],y=diff(y))
cc<-data.frame(x1=c(x[1:mti],rep(NA,length(x[-c(1:mti)]))),pc=c(pc,rep(NA,length(x[-c(1:mti)]))), x=x,y=y)
grid.arrange(

ggplot(cc)+geom_point(aes(x=x1,y=pc))+geom_line(aes(x=x,y=y),col="red")+ylab("Deaths")+xlab("Days since 24/2")+labs(title="Deaths with Covid-19 in Italy"),

ggplot(cc1)+geom_point(aes(x=x1,y=pc))+geom_line(aes(x=x,y=y),col="red")+ylab("New deaths")+xlab("Days since 24/2")#+labs(title="Covid-19 cases in Lombardia Region")
)
dev.off()
# B + (T - B)/[1 + 1
