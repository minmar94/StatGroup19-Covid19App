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

source("Script/UsefulFuns.R")
load("Data/ICU/ICUtomorrow.RData")
outmod_terapie_plot <- outmod_terapie_tab

# Read regional data up to today
dati_reg <- read_regional(path = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")

# Leggo dati residenti
residents <- read.csv("Data/residenti2019.csv",header=TRUE,sep=",")
residents[,1] <- as.character(residents[,1])
residents[10,1] <- "Valle d'Aosta"

# Modello terapia intensiva
icu_data <- dataprep_terapie(dftoprep = dati_reg %>% spread(Key, Value) %>% arrange(denominazione_regione), resdata = residents)

outmod_terapie_tab <- modello_terapia_intensiva(dat = icu_data$da, dattopred = icu_data$dapred)


save(outmod_terapie_plot, outmod_terapie_tab, file = "Data/ICU/ICUOutput.RData")
