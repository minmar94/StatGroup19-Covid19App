
dati=read.table("dpc-covid19-ita-regioni.csv",header=T,sep=",")

dati$denominazione_regione=factor(dati$denominazione_regione)

levels(dati$denominazione_regione)[12:13]="TrentinoAltoAdige"

fa=factor(dati[,1])
fa2=factor(dati[,4])
ag=aggregate(dati[,c(7:16)],by=list(fa,fa2),sum)

names(ag)[1:2]=c("Data","Regione")
dati=ag
save(dati,file="DatiRegione1Apr_recode.RData")

