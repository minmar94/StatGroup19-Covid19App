#setwd("~/Dropbox/StatGroup19")
#load("RData/DatiRegione25Mar_recode.RData")
require(ggplot2)
DatiReg27Mar$time<-unclass(DatiReg27Mar$Data)
#Vall D +Basilicata +Cal+Molise+Sard
#Abruzzo+Campa+Friuli+Lazio+Trent+Pug+Sici+Umb
#Liguria+Toscana+Marche
#Piemonte+Veneto+Emilia
#Lombardia
### organizzazione dei dataframe per i plot
cc1<-c(grep("Valle dAosta",DatiReg27Mar$Regione),grep("Lombardia",DatiReg27Mar$Regione),grep("Liguria",DatiReg27Mar$Regione),grep("Piemonte",DatiReg27Mar$Regione))
cc2<-c(grep("Emilia Romagna",DatiReg27Mar$Regione),grep("Veneto",DatiReg27Mar$Regione),grep("Friuli Venezia Giulia",DatiReg27Mar$Regione), grep("TrentinoAltoAdige",DatiReg27Mar$Regione))
cc3<-c(grep("Umbria",DatiReg27Mar$Regione),grep("Toscana",DatiReg27Mar$Regione), grep("Marche",DatiReg27Mar$Regione),grep("Lazio",DatiReg27Mar$Regione))
cc4<-c(grep("Abruzzo",DatiReg27Mar$Regione),grep("Molise",DatiReg27Mar$Regione), grep("Campania",DatiReg27Mar$Regione),
       grep("Puglia",DatiReg27Mar$Regione), grep("Basilicata",DatiReg27Mar$Regione), grep("Calabria",DatiReg27Mar$Regione),
       grep("Sicilia",DatiReg27Mar$Regione), grep("Sardegna",DatiReg27Mar$Regione))
cc5<-c(grep("Lombardia",DatiReg27Mar$Regione))

dd1<-DatiReg27Mar[cc1,]
dd2<-DatiReg27Mar[cc2,]
dd3<-DatiReg27Mar[cc3,]
dd4<-DatiReg27Mar[cc4,]
dd5<-DatiReg27Mar[cc5,]
dd1$Regione<-factor(dd1$Regione)
dd2$Regione<-factor(dd2$Regione)
dd3$Regione<-factor(dd3$Regione)
dd4$Regione<-factor(dd4$Regione)
dd5$Regione<-factor(dd5$Regione)

require(mgcv)

## stima del trend del gruppo
mod1<-gam(terapia_intensiva~s(time),data=dd1)
dd1$fit<-fitted(mod1)
mod2<-gam(terapia_intensiva~s(time),data=dd2)
dd2$fit<-fitted(mod2)
mod3<-gam(terapia_intensiva~s(time),data=dd3)
dd3$fit<-fitted(mod3)
mod4<-gam(terapia_intensiva~s(time),data=dd4)
dd4$fit<-fitted(mod4)
mod5<-gam(terapia_intensiva~s(time),data=dd5)
dd5$fit<-fitted(mod5)

require(gridExtra)
pdf("Grafici/ICU_regioni/gruppo1.pdf")
grid.arrange(
ggplot(dd1)+geom_line(aes(x=time,y=terapia_intensiva,col=Regione))+geom_point(aes(x=time,y=terapia_intensiva,col=Regione))+geom_smooth(aes(x=time,y=fit),col="darkblue")+scale_x_continuous("giorni",breaks=dd1$time,labels = substring(as.character(dd1$Data),6,10))+scale_y_continuous("Ricoveri in terapia intensiva")+theme(axis.text.x = element_text(angle = 90)),
ggplot(dd2)+geom_line(aes(x=time,y=terapia_intensiva,col=Regione))+geom_point(aes(x=time,y=terapia_intensiva,col=Regione))+geom_smooth(aes(x=time,y=fit),col="darkblue")+scale_x_continuous("giorni",breaks=dd1$time,labels = substring(as.character(dd1$Data),6,10))+scale_y_continuous("Ricoveri in terapia intensiva")+theme(axis.text.x = element_text(angle = 90)))
dev.off()
pdf("Grafici/ICU_regioni/gruppo2.pdf")
grid.arrange(
ggplot(dd3)+geom_line(aes(x=time,y=terapia_intensiva,col=Regione))+geom_point(aes(x=time,y=terapia_intensiva,col=Regione))+geom_smooth(aes(x=time,y=fit),col="darkblue")+scale_x_continuous("giorni",breaks=dd1$time,labels = substring(as.character(dd1$Data),6,10))+scale_y_continuous("Ricoveri in terapia intensiva")+theme(axis.text.x = element_text(angle = 90)),
ggplot(dd4)+geom_line(aes(x=time,y=terapia_intensiva,col=Regione))+geom_point(aes(x=time,y=terapia_intensiva,col=Regione))+geom_smooth(aes(x=time,y=fit),col="darkblue")+scale_x_continuous("giorni",breaks=dd1$time,labels = substring(as.character(dd1$Data),6,10))+scale_y_continuous("Ricoveri in terapia intensiva")+theme(axis.text.x = element_text(angle = 90))
)
dev.off()
pdf("Grafici/ICU_regioni/lombardia.pdf")
ggplot(dd5)+geom_smooth(aes(x=time,y=fit),col="darkblue")+geom_line(aes(x=time,y=terapia_intensiva,col=Regione))+geom_point(aes(x=time,y=terapia_intensiva,col=Regione))+scale_x_continuous("giorni",breaks=dd1$time,labels = substring(as.character(dd1$Data),6,10))+scale_y_continuous("Ricoveri in terapia intensiva")+theme(axis.text.x = element_text(angle = 90))
dev.off()
### grafici con le linee
################# solo smooth
pdf("Grafici/ICU_regioni/gruppo1_smooth_chiaro.pdf")
grid.arrange(
  ggplot(dd1)+geom_smooth(aes(x=time,y=terapia_intensiva,col=Regione),se=F)+scale_x_continuous("giorni",breaks=dd1$time,labels = substring(as.character(dd1$Data),6,10))+scale_y_continuous("Ricoveri in terapia intensiva")+theme_light()+theme(axis.text.x = element_text(angle = 90))+geom_point(aes(x=time,y=terapia_intensiva,col=Regione)),
  ggplot(dd2)+geom_smooth(aes(x=time,y=terapia_intensiva,col=Regione),se=F)+scale_x_continuous("giorni",breaks=dd1$time,labels = substring(as.character(dd1$Data),6,10))+scale_y_continuous("Ricoveri in terapia intensiva")+theme_light()+theme(axis.text.x = element_text(angle = 90))+geom_point(aes(x=time,y=terapia_intensiva,col=Regione)))
dev.off()
pdf("Grafici/ICU_regioni/gruppo2_smooth_chiaro.pdf")
grid.arrange(
  ggplot(dd3)+geom_smooth(aes(x=time,y=terapia_intensiva,col=Regione),se=F)+scale_x_continuous("giorni",breaks=dd1$time,labels = substring(as.character(dd1$Data),6,10))+scale_y_continuous("Ricoveri in terapia intensiva")+theme(axis.text.x = element_text(angle = 90))+geom_point(aes(x=time,y=terapia_intensiva,col=Regione)),
  ggplot(dd4)+geom_smooth(aes(x=time,y=terapia_intensiva,col=Regione),se=F)+scale_x_continuous("giorni",breaks=dd1$time,labels = substring(as.character(dd1$Data),6,10))+scale_y_continuous("Ricoveri in terapia intensiva")+theme(axis.text.x = element_text(angle = 90))+geom_point(aes(x=time,y=terapia_intensiva,col=Regione))
)
dev.off()
pdf("Grafici/ICU_regioni/lombardia_smooth_chiaro.pdf")
ggplot(dd5)+geom_smooth(aes(x=time,y=terapia_intensiva,col=Regione),se=F)+scale_x_continuous("giorni",breaks=dd1$time,labels = substring(as.character(dd1$Data),6,10))+scale_y_continuous("Ricoveri in terapia intensiva")+theme_light()+theme(axis.text.x = element_text(angle = 90))+geom_point(aes(x=time,y=terapia_intensiva,col=Regione))
dev.off()
require(mgcv)
mod1<-gam(terapia_intensiva~s(time),data=dd1)
dd1$fit<-fitted(mod1)
mod2<-gam(terapia_intensiva~s(time),data=dd2)
dd2$fit<-fitted(mod2)
mod3<-gam(terapia_intensiva~s(time),data=dd3)
dd3$fit<-fitted(mod3)
mod4<-gam(terapia_intensiva~s(time),data=dd4)
dd4$fit<-fitted(mod4)
mod5<-gam(terapia_intensiva~s(time),data=dd5)
dd5$fit<-fitted(mod5)

plot(mod1)
ggplot(dd1)+geom_line(aes(x=time,y=terapia_intensiva,col=Regione))+geom_point(aes(x=time,y=terapia_intensiva,col=Regione))+geom_smooth(aes(x=time,y=fit),col="darkblue")

ggplot(dd2)+geom_line(aes(x=time,y=terapia_intensiva,col=Regione))+geom_point(aes(x=time,y=terapia_intensiva,col=Regione))+geom_smooth(aes(x=time,y=fit),col="darkblue")
ggplot(dd3)+geom_line(aes(x=time,y=terapia_intensiva,col=Regione))+geom_point(aes(x=time,y=terapia_intensiva,col=Regione))+geom_smooth(aes(x=time,y=fit),col="darkblue")
ggplot(dd4)+geom_line(aes(x=time,y=terapia_intensiva,col=Regione))+geom_point(aes(x=time,y=terapia_intensiva,col=Regione))+geom_smooth(aes(x=time,y=fit),col="darkblue")
ggplot(dd5)+geom_line(aes(x=time,y=terapia_intensiva,col=Regione))+geom_point(aes(x=time,y=terapia_intensiva,col=Regione))+geom_smooth(aes(x=time,y=fit),col="darkblue")


######### differenze
plotdif<-function(dd1){
    ll<-levels(dd1$Regione)
    mx=max(dd1$time)
    mmx=max(dd1$time)-1
dd1.dif<-data.frame(Data=factor(rep(dd1$Data[2:mx],length=length(ll)*mmx)),Regione=factor(rep(ll,each=mmx)),terapia_intensiva=unlist(tapply(dd1$terapia_intensiva,dd1$Regione,diff)), time=rep(c(2:mx),length=length(ll)*mmx))
ggplot(dd1.dif)+geom_smooth(aes(x=time,y=terapia_intensiva,col=Regione),se = F)+geom_point(aes(x=time,y=terapia_intensiva,col=Regione))+scale_x_continuous("giorni",breaks=dd1$time,labels = substring(as.character(dd1$Data),6,10))+scale_y_continuous("Nuovi ricoveri in terapia intensiva")+theme_light()+theme(axis.text.x = element_text(angle = 90))#+ylim(-15,22)#+geom_smooth(aes(x=time,y=fit),col="darkblue")
}
#pdf("Grafici/nuoviricoveri_1.pdf")
pdf("Grafici/NO.pdf")
plotdif(dd1)
dev.off()
pdf("Grafici/NE.pdf")
plotdif(dd2)
dev.off()
pdf("Grafici/Centro.pdf")
plotdif(dd3)
dev.off()
pdf("Grafici/SudIsole.pdf")
plotdif(dd4)
dev.off()
# plotdif(dd5)
# dev.off()
