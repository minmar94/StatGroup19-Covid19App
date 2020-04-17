# Packages ----------------------------------------------------------------

library(MASS)
library(rmutil)
library(GA)
library(nplr)
library(lqmm)
library(numDeriv)
require(tidyverse)
require(magrittr)

# Functions ---------------------------------------------------------------

Richards <- function(ti,pars)
{
  den <- (1 + 10^(pars[3] * (exp(pars[4]) - ti)))^exp(pars[5])
  out <- exp(pars[1]) + exp(pars[2])/den
  
  return(out)
}     

# Loading data ---------------------------------------------------------

load("Data/DatiPiero.RData")
dat <- tibble(cumCounts = data_formodel_prep$`Totale casi`, 
              time = data_formodel_prep$ti, 
              regione = data_formodel_prep$region) %>% group_by(time) %>% 
  summarise(cumCounts=sum(cumCounts))

ts <- dat$time
nts <- length(ts)
cumCounts <- dat$cumCounts
plot(ts, cumCounts, col=2, pch=19, type="b")

# Building likelihood -----------------------------------------------------

# Likelihood
likPois <- function(pars, ti, cc) 
{
  linPred <- Richards(ti, pars)
  out <- sum(dpois(cc, linPred, log=T))
  
  return(out)
}

logPoisGradient <- function(pars, ti, cc)
{
  
  b <- exp(pars[1])
  r <- exp(pars[2])
  h <- pars[3]
  p <- exp(pars[4])
  s <- exp(pars[5])
  
  lambdat <- Richards(ti = ti, pars = pars)
  dlb <- rep(1, length(ti))*b
  dlr <- (1+10^(h*(p-ti)))^(-s)*r
  dlh <- -r*s*log(10)*(p-ti)*(1+10^(h*(p-ti)))^(-s-1)*10^(h*(p-ti))
  dlp <- -r*s*log(10)*h*(1+10^(h*(p-ti)))^(-s-1)*10^(h*(p-ti))*p
  dls <- -r*(1+10^(h*(p-ti)))^(-s)*log(1+10^(h*(p-ti)))*s
  
  db <- - sum(dlb) + sum(cc*1/lambdat*dlb)
  dr <- - sum(dlr) + sum(cc*1/lambdat*dlr)
  dh <- - sum(dlh) + sum(cc*1/lambdat*dlh)
  dp <- - sum(dlp) + sum(cc*1/lambdat*dlp)
  ds <- - sum(dls) + sum(cc*1/lambdat*dls)
  
  out <- c(db, dr, dh, dp, ds)
  
  return(out)
}

# Maximizing likelihood ---------------------------------------------------

# Kernel smooth data (ho usato un mirroring boundary correction)
dd <- density(c(rep(ts, cumCounts), nts+(nts-rep(ts, cumCounts))), from = 1, to = nts)
countSmooth <- dd$y*2*sum(cumCounts)
plot(dd$x, countSmooth, type="l", lwd=2)

# Starting parameters for optim
p1 <- min(countSmooth)
p2 <- max(countSmooth)-min(countSmooth)
np <- as.vector(unlist(nplr(dd$x, countSmooth, 
                            useLog=F, npars=5)@pars)) # Non so benissimo come funzioni la funzione nplr, ma dandogli i valori effettivi (nonstante il warning) senza convertirili in proporzioni mi sembra restituisca fuori valori più ragionevoli

inits <- c(log(p1), log(p2), np[4], log(np[3]), log(np[5]))

# Optim
inits2 <- optim(inits, function(x) -likPois(x, cc=cumCounts, ti=ts), 
                method="BFGS")$par

# Genetic algorithm
maxiter=1e4
runs=500
rg <- ga("real-valued", function(x) likPois(x, cc=cumCounts, ti=ts), 
         lower=rep(-20,length(inits)), upper=rep(20,length(inits)),
         maxiter=maxiter/2, run=runs, optim=TRUE, 
         suggestions=rbind(inits, inits2))

rg2 <- ga("real-valued", function(x) likPois(x, cc=cumCounts, ti=ts), 
          lower=rep(-20,length(inits)), upper=rep(20,length(inits)), 
          maxiter=maxiter/2, run=runs, optim=TRUE)

rg <- ga("real-valued", function(x)  likPois(x, cc=cumCounts, ti=ts), 
         lower=rep(min(rg@solution)*1.5,length(inits)), 
         upper=rep(max(rg@solution)*1.5,length(inits)), 
         maxiter=maxiter, run=runs, optim=TRUE, 
         suggestions=rbind(rg@solution,rg2@solution), 
         optimArgs=list(control=list(maxit=1000)))

GApars <- rg@solution[1,]

# Plotting resulting estimated curve --------------------------------------

curve(Richards(x, pars=GApars), lwd=2, col=2, xlim=c(0, nts))

# Hessiana ----------------------------------------------------------------

# Le due hessiane vengono sempre molto simili, e raramente positive definite
# Per qualche motivo, molto spesso, la funzione make.positive.definite non riesce a rendere la seconda hessiana definita positiva
# Quando positiva definita l'hessiana derivata dal gradiente produce SE più bassi

hess <- hessian(function(x) likPois(x, cc=countSim, ti=ts), 
                GApars)
(conv <- is.positive.definite(hess))

hess2 <- jacobian(function(x) logPoisGradient(x, ti=ts, cc=countSim), 
                  GApars)
(conv2 <- is.positive.definite(hess2))

if(!conv) 
{
  hess <- make.positive.definite(hess)
}
if(!conv2) 
{
  hess2 <- make.positive.definite(hess2)
}
(se <- sqrt(diag(solve(hess))))
(se2 <- sqrt(diag(solve(hess2))))

# Di solito, la seconda (basata su gradiente) produce SE più bassi
rbind(GApars-1.96*se,
      GApars,
      GApars+1.96*se)
rbind(GApars-1.96*se2,
      GApars,
      GApars+1.96*se2)

# Fit of the curve on the data --------------------------------------------

linPred <- Richards(ts, GApars)
plot(linPred, cex=0.5, col=2, lwd=2, type="l")
points(ts, countSim, type="b", pch=19)



# Fit of the curve on the data --------------------------------------------

linPred <- Richards(ts, GApars)
plot(linPred, cex=0.5, col=2, lwd=2, type="l")
points(ts, cumCounts, type="b", pch=19)
