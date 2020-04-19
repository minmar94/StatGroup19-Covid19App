# Packages ----------------------------------------------------------------

library(MASS)
library(rmutil)
library(GA)
library(nplr)
library(lqmm)
library(numDeriv)
require(tidyverse)
require(magrittr)

source("FunctionsDef.R")

# Loading data ---------------------------------------------------------

load("Data/Dati16_04.RData")
dat <- tibble(cumCounts = data_formodel_prep$`Variazione totale positivi`, 
              time = data_formodel_prep$ti, 
              regione = data_formodel_prep$region) %>% group_by(time) %>% 
  summarise(cumCounts=sum(cumCounts))

ts <- dat$time
nts <- length(ts)
cumCounts <- dat$cumCounts


# Fitting -----------------------------------------------------------------

oo <- growthGLM(count=cumCounts, ti = ts, monotone = T, family="nb", 
                nmirror = 5)

# Fit of the curve on the data --------------------------------------------

linPred <- oo$linPred
plot(linPred, cex=0.5, col=2, lwd=2, type="l")
points(ts, cumCounts, type="b", pch=19)

(pars <- oo$pars)
(lik <- oo$lik)
(R2 <- oo$R2)
(se <- oo$se)

lpars <- oo$pars-1.96*oo$se
upars <- oo$pars+1.96*oo$se
rbind(lbound=lpars,
      est=pars,
      ubound=upars)

curve(oo$lpFun(pars, x), xlim = c(min(ts), max(ts)), col=1, lty=1, lwd=1)
curve(oo$lpFun(lpars, x), xlim = c(min(ts), max(ts)), col=2, lty=2, lwd=1, add=T)
curve(oo$lpFun(upars, x), xlim = c(min(ts), max(ts)), col=2, lty=2, lwd=1, add=T)
points(ts, cumCounts, type="b", cex=0.5, col=3)

lpars[1] <- pars[1]
upars[1] <- pars[1]
curve(oo$lpFun(pars, x), xlim = c(min(ts), max(ts)), col=1, lty=1, lwd=1)
curve(oo$lpFun(lpars, x), xlim = c(min(ts), max(ts)), col=2, lty=2, lwd=1, add=T)
curve(oo$lpFun(upars, x), xlim = c(min(ts), max(ts)), col=2, lty=2, lwd=1, add=T)
points(ts, cumCounts, type="b", cex=0.5, col=3)
