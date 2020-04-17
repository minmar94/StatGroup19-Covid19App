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

# Definisco la funzione di Richards su scala log per tutti i parametri meno che h
Richards <- function(ti,pars)
{
  den <- (1 + 10^(pars[3] * (exp(pars[4]) - ti)))^exp(pars[5])
  out <- exp(pars[1]) + exp(pars[2])/den
  
  return(out)
}     

# Loading data ---------------------------------------------------------

set.seed(12345)
# Genero conteggi secondo il seguente set di parametri veri
truePars <- c(log(0.1), log(1000), 0.05, log(40), log(3))
nts <- 100
ts <- 1:nts
countSim <- rpois(nts, lambda=Richards(ts, truePars))

curve(Richards(x, truePars), xlim = c(0, nts), lwd=2, col=1, 
      ylim=c(0, max(countSim)))
points(ts, countSim, col=2, pch=19, type="b")

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


# Check derivative by derivative ------------------------------------------

# Nel seguito, verifichiamo che fissati quattro parametri su cinque su un valore (in particolare il valore vero dei dati simulati), il gradiente del parametro libero abbia un comportamento coerente con la likelihood marginale rispetto a quel parametro

# Likelihood marginale rispetto al p-esimo parametro
OnelikPois <- function(x, p) 
{
  newp <- truePars
  newp[p] <- x
  linPred <- Richards(ts, newp)
  out <- sum(dpois(countSim, linPred, log=T))
  
  return(out)
}

# p-esima componente del gradiente
oneGradFun <- function(x, p)
{
  newp <- truePars
  newp[p] <- x
  out <- logPoisGradient(newp, ts, countSim) 
  return(out[p])
}

vOnelikPois <- Vectorize(OnelikPois)
vOneGradFun <- Vectorize(oneGradFun)

# Andando parametro per parametro
# Prima verifichiamo il comportamento generale: derivata positiva quando la lik cresce e negativa quando decresce. 

# Dopo verifichiamo che la derivata si annulli nel punto di massimo (trovato numericamente)
# Checking derivative w.r.t. b ---------------------------------------------

# Il comportamento di likelihood marginale e derivata rispetto a b è strano
layout(matrix(c(1,2), ncol=2))
curve(vOnelikPois(x, p=1), xlim=c(-5, 10), xlab=expression(log(b)), ylab="logLik",
      main="Behavior loglik")
curve(vOneGradFun(x, p=1), xlim=c(-5, 10), xlab=expression(log(b)), ylab="d/db logLik",
      main="Behavior derivative")
layout(matrix(c(1)))

# La derivata sembra comunque annullarsi al punto di massimo
xSeq <- seq(-5, 10, length.out = 1000)
max.point <- xSeq[which.max(vOnelikPois(xSeq, p=1))]
curve(vOneGradFun(x, p=1), xlim=c(max.point-3, max.point+3), 
      xlab=expression(log(b)), ylab="d/db logLik",
      main="Derivative around p. of Maximum")
abline(v=max.point, col=2)
abline(h=0, col=2)

# Checking derivative w.r.t. r ---------------------------------------------

# Behavior
layout(matrix(c(1,2), ncol=2))
curve(vOnelikPois(x, p=2), xlim=c(-5, 10), 
      xlab=expression(log(r)), ylab="logLik",
      main="Behavior loglik")
curve(vOneGradFun(x, p=2), xlim=c(-5, 10), 
      xlab=expression(log(r)), ylab="d/dr logLik",
      main="Behavior derivative")
layout(matrix(c(1)))

# Does point of maximum has derivative = 0
xSeq <- seq(-10, 10, length.out = 1000)
max.point <- xSeq[which.max(vOnelikPois(xSeq, p=2))]
curve(vOneGradFun(x, p=2), xlim=c(max.point-3, max.point+3), 
      xlab=expression(log(r)), ylab="d/dr logLik",
      main="Derivative around p. of Maximum")
abline(v=max.point, col=2)
abline(h=0, col=2)

# Checking derivative w.r.t. h ---------------------------------------------

# In h=0 c'è un flesso! La derivata è infinito
# Behavior
layout(matrix(c(1,2), ncol=2))
curve(vOnelikPois(x, p=3), xlim=c(-.5, .5), 
      xlab=expression(log(h)), ylab="logLik",
      main="Behavior loglik")
curve(vOneGradFun(x, p=3), xlim=c(-.5, .5), 
      xlab=expression(log(h)), ylab="d/dh logLik",
      main="Behavior derivative")
layout(matrix(c(1)))

# Does point of maximum has derivative = 0
xSeq <- seq(-10, 10, length.out = 1000)
max.point <- xSeq[which.max(vOnelikPois(xSeq, p=3))]
curve(vOneGradFun(x, p=3), xlim=c(max.point-0.5, max.point+0.5), 
      xlab=expression(log(h)), ylab="d/dh logLik",
      main="Derivative around p. of Maximum")
abline(v=max.point, col=2)
abline(h=0, col=2)

# Checking derivative w.r.t. p ---------------------------------------------

# Behavior
layout(matrix(c(1,2), ncol=2))
curve(vOnelikPois(x, p=4), xlim=c(-5, 4), 
      xlab=expression(log(p)), ylab="logLik",
      main="Behavior loglik")
curve(vOneGradFun(x, p=4), xlim=c(-5, 4), 
      xlab=expression(log(p)), ylab="d/dp logLik",
      main="Behavior derivative")
layout(matrix(c(1)))

# Does point of maximum has derivative = 0
xSeq <- seq(-10, 10, length.out = 1000)
max.point <- xSeq[which.max(vOnelikPois(xSeq, p=4))]
curve(vOneGradFun(x, p=4), xlim=c(max.point-3, max.point+3), 
      xlab=expression(log(p)), ylab="d/dp logLik",
      main="Derivative around p. of Maximum")
abline(v=max.point, col=2)
abline(h=0, col=2)

# Checking derivative w.r.t. s ---------------------------------------------

# Behavior
layout(matrix(c(1,2), ncol=2))
curve(vOnelikPois(x, p=5), xlim=c(-5, 10), 
      xlab=expression(log(s)), ylab="logLik",
      main="Behavior loglik")
curve(vOneGradFun(x, p=5), xlim=c(-5, 10), 
      xlab=expression(log(s)), ylab="d/ds logLik",
      main="Behavior derivative")
layout(matrix(c(1)))

# Does point of maximum has derivative = 0
xSeq <- seq(-10, 10, length.out = 1000)
max.point <- xSeq[which.max(vOnelikPois(xSeq, p=5))]
curve(vOneGradFun(x, p=5), xlim=c(max.point-3, max.point+3), 
      xlab=expression(log(s)), ylab="d/ds logLik",
      main="Derivative around p. of Maximum")
abline(v=max.point, col=2)
abline(h=0, col=2)

