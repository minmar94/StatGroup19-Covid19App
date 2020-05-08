# Packages ----------------------------------------------------------------

library(MASS)
library(rmutil)
library(GA)
library(nplr)
library(lqmm)
library(numDeriv)
require(tidyverse)
require(magrittr)
require(mvtnorm)
require(corpcor)

# Functions - Richard Monotone-----------------------------------------------------

# Linear Predictor monotone
Richards <- function(pars, ti, bFix=0)
{
  logr <- pars[1]
  h <- exp(pars[2])
  p <- pars[3]
  s <- exp(pars[4])
  
  logden <- s*log(1 + 10^(h * (p - ti)))
  lout <- log(1e-10+exp(logr-logden))
  
  return(lout)
}

# Derivate prime
d1Rich <- function(pars, ti)
{
  logr <- pars[1]
  h <- exp(pars[2])
  p <- pars[3]
  s <- exp(pars[4])
  
  dlr <- exp(-s*log(1+10^(h*(p-ti))) + logr)
  dlh <- -log(10)*(p-ti)*exp(logr+log(s)+(-s-1)*log(1+10^(h*(p-ti)))+
                               (h*(p-ti))*log(10)+log(h))
  dlp <- -log(10)*h*exp(logr+log(s)+(-s-1)*log(1+10^(h*(p-ti)))+(h*(p-ti))*log(10))
  dls <- -log(1+10^(h*(p-ti)))*exp(logr-s*log(1+10^(h*(p-ti)))+log(s))
  
  out <- matrix(c(dlr, dlh, dlp, dls), nrow=length(ti))
  
  return(out)
}

# Derivate seconde
d2Rich <- function(pars, ti)
{
  logr <- pars[1]
  h <- exp(pars[2])
  p <- pars[3]
  s <- exp(pars[4])
  
  d1lt <- d1Rich(ti = ti, pars = pars)
  
  dlr <- d1lt[, 1]
  dlh <- d1lt[, 2]
  dlp <- d1lt[, 3]
  dls <- d1lt[, 4]
  
  aux1 <- (1+10^(h*(p-ti)))
  aux2 <- 10^(h*(p-ti))
  laux2 <- (h*(p-ti))*log(10)
  
  dlrr <- dlr + rep(0, length(ti))
  dlrh <- -log(10)*(p-ti)*exp(log(s)+(-s-1)*log(aux1)+laux2+logr+log(h))
  dlrp <- -log(10)*exp(log(h)+log(s)+(-s-1)*log(aux1)+laux2+logr)
  dlrs <- -log(aux1)*exp(-s*log(aux1)+logr+log(s))
  
  dlhh <- dlh-(p-ti)^2*(log(10))^2*exp(logr+log(s)+laux2+(-s-1)*log(aux1)+2*log(h))*
    (-exp(log(s+1)-log(aux1)+laux2)+1)
  
  dlhp <- -log(10)*exp(logr+log(s)+laux2+(-s-1)*log(aux1)+log(h))*
    (-log(10)*(p-ti)*exp(log(s+1)+log(h)-log(aux1)+laux2)+h*log(10)*(p-ti)+1)
  dlhs <- -log(10)*(p-ti)*exp(logr+laux2+(-s-1)*log(aux1)+log(s)+log(h))*
    (1-s*log(aux1))
  
  dlpp <- - exp(log(log(10)^2)+2*log(h)+logr+log(s)+laux2+(-s-1)*log(aux1))*
    (-exp(log(s+1)-log(aux1)+log(aux2))+1)
  dlps <- -log(10)*exp(log(h)+logr+laux2+(-s-1)*log(aux1)+log(s))*(1-s*log(aux1))
  
  dlss <- dls + (log(aux1))^2*exp(logr-s*log(aux1)+2*log(s))
  
  
  out <- matrix(c(dlrr, dlrh, dlrp, dlrs, 
                  dlhh, dlhp, dlhs,
                  dlpp, dlps,
                  dlss), nrow=length(ti))
  
  return(out)
}

# Functions - Richard Mirrored -----------------------------------------------------

# Linear Predictor mirrored
mirrRich <- function(pars, ti, tk, npar=8) 
{
  if (npar==5)
  {
    logr <- pars[1]
    
    hill <- ifelse(ti<tk, pars[2], pars[3])
    symm <- exp(pars[4])
    
    logden <- symm*log(1+10^(hill*(ti-tk)^2))
  }
  
  if (npar==8)
  {
    lr1 <- pars[1]
    
    h1 <- -exp(pars[2])
    h2 <- -exp(pars[3])
    hill <- ifelse(ti<tk, h1, h2)
    
    s1 <- exp(pars[4])
    s2 <- exp(pars[5])
    symm <- ifelse(ti<tk, s1, s2)
    
    p1 <- pars[6]
    p2 <- pars[7]
    peak <- ifelse(ti<tk, p1, p2)
    
    lr2 <- lr1+s2*log(1+10^(h2*p2))-s1*log(1+10^(h1*p1))
    logr <- ifelse(ti<tk, lr1, lr2)
    
    logden <- symm*log((1+10^(hill*(peak-(ti-tk)^2))))
  }
  
  if (!(npar%in%c(5, 8)))
  {
    stop("Mirror Richards is only available with 5 pars or 8 pars")
  }
  
  lout <- log(1e-10 + exp(logr-logden))
  
  return(lout)
}

# Derivate prime
d1MRich5 <- function(pars, ti, tk)
{
  logr <- pars[1]
  h1 <- pars[2]
  h2 <- pars[3]
  s <- exp(pars[4])
  
  t1 <- ti[ti<tk]
  t2 <- ti[ti>=tk]
  
  # Auxiliary
  tilde1 <- (t1-tk)^2
  tilde2 <- (t2-tk)^2
  l1ht <- 10^(h1*tilde1)
  l2ht <- 10^(h2*tilde2)
  ll1ht <- (h1*tilde1)*log(10)
  ll2ht <- (h2*tilde2)*log(10)
  l1aux <- (1+l1ht)
  l2aux <- (1+l2ht)
  
  dl1r <- exp(-s*log(l1aux) + logr)
  dl2r <- exp(-s*log(l2aux) + logr)
  
  dl1h1 <- - log(10) * exp(logr + log(s) + (-s-1)*log(l1aux) + ll1ht + log(tilde1))
  dl2h1 <- rep(0, length(t2))
  
  dl1h2 <- rep(0, length(t1))
  dl2h2 <- -log(10) * exp(logr+log(s)+(-s-1)*log(l2aux)+ll2ht+log(tilde2))
  
  dl1s <- -log(l1aux) * exp(logr - s*log(l1aux) + log(s))
  dl2s <- -log(l2aux) * exp(logr - s*log(l2aux) + log(s))
  
  out1 <- matrix(c(dl1r, dl1h1, dl1h2, dl1s), nrow=length(t1), ncol=length(pars))
  out2 <- matrix(c(dl2r, dl2h1, dl2h2, dl2s), 
                 nrow=length(t2), ncol=length(pars))
  
  return(list(out1, out2))
}

# Derivate prime
# Derivate prime
d1MRich8 <- function(pars, ti, tk)
{
  logr1 <- pars[1]
  h1 <- -exp(pars[2])
  h2 <- -exp(pars[3])  
  s1 <- exp(pars[4])
  s2 <- exp(pars[5])
  p1 <- pars[6]
  p2 <- pars[7]
  
  logr2 <- logr1+s2*log(1+10^(h2*p2))-s1*log(1+10^(h1*p1))
  
  t1 <- ti[ti<tk]
  t2 <- ti[ti>=tk]
  
  # Auxiliary
  tilde1 <- (t1-tk)^2
  tilde2 <- (t2-tk)^2
  pmt1 <- p1-tilde1
  pmt2 <- p2-tilde2
  l1hp <- 10^(h1*pmt1)
  l2hp <- 10^(h2*pmt2)
  ll1hp <- (h1*pmt1)*log(10)
  ll2hp <- (h2*pmt2)*log(10)
  l1aux <- (1+l1hp)
  l2aux <- (1+l2hp)
  
  dl1r1 <- exp(-s1*log(l1aux)+logr1)
  dl2r1 <- exp(s2*log(1+10^(h2*p2))-s1*log(1+10^(h1*p1))-s2*log(l2aux)+logr1)
  
  dl1h1 <- log(10)*pmt1*h1*exp(logr1+log(s1)+(-s1-1)*log(l1aux)+ll1hp)
  dl2h1 <- log(10)*h1*p1*exp(logr1+s2*log(1+10^(h2*p2))-s2*log(l2aux) +log(s1)+ 
                            (-s1-1)*log(1+10^(h1*p1))+(h1*p1)*log(10))
  
  dl1h2 <- rep(0, length(t1))
  dl2h2 <- log(10)*p2*h2*exp(logr1-s1*log(1+10^(h1*p1))+
                            s2*log(1+10^(h2*p2))-s2*log(l2aux)+log(s2))*
    (exp(-log(l2aux)+ll2hp)*pmt2-exp(-log(1+10^(h2*p2))+(h2*p2)*log(10)))
  
  dl1s1 <- -log(l1aux) * exp(logr1 -s1*log(l1aux)+log(s1))
  dl2s1 <- -log(1+10^(h1*p1))*exp(logr1+s2*log(1+10^(h2*p2))-
                                    s2*log(l2aux)-s1*log(1+10^(h1*p1))+log(s1))
  
  dl1s2 <- rep(0, length(t1))
  dl2s2 <- (log(1+10^(h2*p2))-log(l2aux)) * 
    exp(logr1-s1*log(1+10^(h1*p1))+s2*log(1+10^(h2*p2))-s2*log(l2aux)+log(s2))
  
  
  dl1p1 <- -h1*log(10)*exp(logr1+log(s1)+(-s1-1)*log(l1aux)+ll1hp)
  dl2p1 <- -h1*log(10)*exp(logr1+s2*log(1+10^(h2*p2))-s2*log(l2aux)+log(s1)+
                             (-s1-1)*log(1+10^(h1*p1))+(h1*p1)*log(10))
  
  dl1p2 <- rep(0, length(t1))
  dl2p2 <- -h2*log(10) * 
    exp(logr1-s1*log(1+10^(h1*p1))+s2*log(1+10^(h2*p2))-s2*log(l2aux)+log(s2)) *
    (-exp(-log(l2aux)+ll2hp) + exp(-log(1+10^(h2*p2))+(h2*p2)*log(10)))
  
  out1 <- matrix(c(dl1r1, dl1h1, dl1h2, dl1s1, dl1s2, dl1p1, dl1p2), 
                 nrow=length(t1), ncol=length(pars))
  out2 <- matrix(c(dl2r1, dl2h1, dl2h2, dl2s1, dl2s2, dl2p1, dl2p2), 
                 nrow=length(t2), ncol=length(pars))
  
  return(list(out1, out2))
}
# Functions - Poisson -----------------------------------------------------

# Likelihood
likPois <- function(pars, ti, cc, lp) 
{
  linPred <- exp(lp(pars=pars, ti=ti))
  out <- sum(dpois(cc, linPred, log=T))
  
  return(out)
}

# Functions - Poisson Richard -----------------------------------------------------

# Gradient Richard-Poisson
PoisRichGradient <- function(pars, ti, cc, lp)
{
  # r <- exp(pars[1])
  # h <- exp(pars[2])
  # p <- exp(pars[3])
  # s <- exp(pars[4])
  
  llambdat <- lp(pars=pars, ti=ti)
  lambdat <- exp(llambdat)
  d1lt <- d1Rich(ti = ti, pars = pars)
  
  dlr <- d1lt[, 1]
  dlh <- d1lt[, 2]
  dlp <- d1lt[, 3]
  dls <- d1lt[, 4]
  
  dr <- - sum(dlr) + sum(exp(log(cc)-llambdat)*dlr)
  dh <- - sum(dlh) + sum(exp(log(cc)-llambdat)*dlh)
  dp <- - sum(dlp) + sum(exp(log(cc)-llambdat)*dlp)
  ds <- - sum(dls) + sum(exp(log(cc)-llambdat)*dls)
  
  out <- c(dr, dh, dp, ds)
  
  return(out)
}

# Hessian Richard-Poisson
PoisRichHessian <- function(pars, ti, cc, lp)
{
  # r <- exp(pars[1])
  # h <- exp(pars[2])
  # p <- exp(pars[3])
  # s <- exp(pars[4])
  
  llambdat <- lp(pars = pars, ti = ti)
  lambdat <- exp(llambdat)
  d1lt <- d1Rich(ti = ti, pars = pars)
  d2lt <- d2Rich(ti = ti, pars = pars)
  
  dlr <- d1lt[, 1]
  dlh <- d1lt[, 2]
  dlp <- d1lt[, 3]
  dls <- d1lt[, 4]
  
  dlrr <- dlr + d2lt[, 1]
  dlrh <- d2lt[, 2]
  dlrp <- d2lt[, 3]
  dlrs <- d2lt[, 4]
  dlhh <- dlh + d2lt[, 5]
  dlhp <- d2lt[, 6]
  dlhs <- d2lt[, 7]
  dlpp <- d2lt[, 8]
  dlps <- d2lt[, 9]
  dlss <- dls + d2lt[, 10]
  
  drr <- -sum(dlrr)+sum(-exp(log(cc)-2*llambdat+log(dlr*dlr))+
                          exp(log(cc)-llambdat)*dlrr)
  drh <- -sum(dlrh)+sum(-exp(log(cc)-2*llambdat)*dlr*dlh+
                          exp(log(cc)-llambdat)*dlrh)
  drp <- -sum(dlrp)+sum(-exp(log(cc)-2*llambdat)*dlr*dlp+
                          exp(log(cc)-llambdat)*dlrp)
  drs <- -sum(dlrs)+sum(-exp(log(cc)-2*llambdat)*dlr*dls+
                          exp(log(cc)-llambdat)*dlrs)
  
  dhh <- -sum(dlhh)+sum(-exp(log(cc)-2*llambdat+log(dlh*dlh))+
                          exp(log(cc)-llambdat)*dlhh)
  dhp <- -sum(dlhp)+sum(-exp(log(cc)-2*llambdat)*dlh*dlp+
                          exp(log(cc)-llambdat)*dlhp)
  dhs <- -sum(dlhs)+sum(-exp(log(cc)-2*llambdat)*dlh*dls+
                          exp(log(cc)-llambdat)*dlhs)
  
  dpp <- -sum(dlpp)+sum(-exp(log(cc)-2*llambdat+log(dlp*dlp))+
                          exp(log(cc)-llambdat)*dlpp)
  dps <- -sum(dlps)+sum(-exp(log(cc)-2*llambdat)*dlp*dls+
                          exp(log(cc)-llambdat)*dlps)
  
  dss <- -sum(dlss)+sum(-exp(log(cc)-2*llambdat+log(dls*dls))+
                          exp(log(cc)-llambdat)*dlss)
  
  out <- matrix(c(drr, drh, drp, drs,
                  drh, dhh, dhp, dhs,
                  drp, dhp, dpp, dps,
                  drs, dhs, dps, dss), ncol=length(pars))
  
  return(out)
}


# Functions MirroredRichards-Poisson --------------------------------------

PoisMRich5Gradient <- function(pars, ti, cc, tk, lp)
{
  # r <- exp(pars[1])
  # h1 <- pars[2]
  # h2 <- pars[3]
  # s <- exp(pars[4])
  
  t1 <- ti[ti<tk]
  t2 <- ti[ti>=tk]
  cc1 <- cc[ti<tk]
  cc2 <- cc[ti>=tk]
  llambdat <- lp(pars = pars, ti = ti)
  llambdat1 <- llambdat[ti<tk]
  llambdat2 <- llambdat[ti>=tk]
  lambdat1 <- exp(llambdat1)
  lambdat2 <- exp(llambdat2)
  d1lt <- d1MRich5(ti=ti, pars=pars, tk=tk)
  d1lt1 <- d1lt[[1]]
  d1lt2 <- d1lt[[2]]
  
  dl1r <- d1lt1[,1]
  dl2r <- d1lt2[,1]
  
  dl1h1 <- d1lt1[,2]
  dl2h1 <- d1lt2[,2]
  
  dl1h2 <- d1lt1[,3]
  dl2h2 <- d1lt2[,3]
  
  dl1s <- d1lt1[,4]
  dl2s <- d1lt2[,4]
  
  dr <- sum(-dl1r + exp(log(cc1)-llambdat1)*dl1r) + 
    sum(-dl2r + exp(log(cc2)-llambdat2)*dl2r)
  
  dh1 <- sum(-dl1h1+exp(log(cc1)-llambdat1)*dl1h1) +
    sum(-dl2h1+exp(log(cc2)-llambdat2)*dl2h1)
  dh2 <- sum(-dl1h2+exp(log(cc1)-llambdat1)*dl1h2) +
    sum(-dl2h2+exp(log(cc2)-llambdat2)*dl2h2)
  
  ds <- sum(-dl1s+exp(log(cc1)-llambdat1)*dl1s) +
    sum(-dl2s+exp(log(cc2)-llambdat2)*dl2s)
  
  out <- c(dr, dh1, dh2, ds)
  
  return(out)
}

# Gradiente Mirrored-Richard-Poisson
PoisMRich8Gradient <- function(pars, ti, cc, tk, lp)
{
  r1 <- exp(pars[1])
  h1 <- -exp(pars[2])
  h2 <- exp(pars[3])  
  s1 <- exp(pars[4])
  s2 <- exp(pars[5])
  p1 <- pars[6]
  p2 <- pars[7]
  
  r2 <- exp(log(r1)+s1*log(1+10^(h2*p2))-s1*log(1+10^(h1*p1)))
  
  t1 <- ti[ti<tk]
  t2 <- ti[ti>=tk]
  cc1 <- cc[ti<tk]
  cc2 <- cc[ti>=tk]
  llambdat <- lp(pars = pars, ti = ti)
  llambdat1 <- llambdat[ti<tk]
  llambdat2 <- llambdat[ti>=tk]
  lambdat1 <- exp(llambdat1)
  lambdat2 <- exp(llambdat2)
  d1lt <- d1MRich8(ti=ti, pars=pars, tk=tk)
  d1lt1 <- d1lt[[1]]
  d1lt2 <- d1lt[[2]]
  
  dl1r1 <- d1lt1[, 1]
  dl2r1 <- d1lt2[, 1]
  
  dl1h1 <- d1lt1[, 2]
  dl2h1 <- d1lt2[, 2]
  
  dl1h2 <- d1lt1[, 3]
  dl2h2 <- d1lt2[, 3]
  
  dl1s1 <- d1lt1[, 4]
  dl2s1 <- d1lt2[, 4]
  
  dl1s2 <- d1lt1[, 5]
  dl2s2 <- d1lt2[, 5]
  
  dl1p1 <- d1lt1[, 6]
  dl2p1 <- d1lt2[, 6]
  
  dl1p2 <- d1lt1[, 7]
  dl2p2 <- d1lt2[, 7]
  
  dr1 <- sum(-dl1r1 + exp(log(cc1)-llambdat1)*dl1r1) + 
    sum(-dl2r1 + exp(log(cc2)-llambdat2)*dl2r1)
  
  dh1 <- sum(-dl1h1+exp(log(cc1)-llambdat1)*dl1h1) +
    sum(-dl2h1+exp(log(cc2)-llambdat2)*dl2h1)
  dh2 <- sum(-dl1h2+exp(log(cc1)-llambdat1)*dl1h2) +
    sum(-dl2h2+exp(log(cc2)-llambdat2)*dl2h2)
  
  ds1 <- sum(-dl1s1+exp(log(cc1)-llambdat1)*dl1s1) +
    sum(-dl2s1+exp(log(cc2)-llambdat2)*dl2s1)
  ds2 <- sum(-dl1s2+exp(log(cc1)-llambdat1)*dl1s2) +
    sum(-dl2s2+exp(log(cc2)-llambdat2)*dl2s2)
  
  dp1 <- sum(-dl1p1+exp(log(cc1)-llambdat1)*dl1p1) +
    sum(-dl2p1+exp(log(cc2)-llambdat2)*dl2p1)
  dp2 <- sum(-dl1p2+exp(log(cc1)-llambdat1)*dl1p2) +
    sum(-dl2p2+exp(log(cc2)-llambdat2)*dl2p2)     
  
  out <- c(dr1, dh1, dh2, ds1, ds2, dp1, dp2)
  
  return(out)
}


# Functions Negative Binomial ---------------------------------------------

# Likelihood
likNB <- function(pars, ti, cc, lp) 
{
  linPred <- exp(lp(pars=pars[-length(pars)], ti=ti))
  out <- sum(dnbinom(cc, size=exp(pars[length(pars)]), mu=linPred, 
                     log=T))
  return(out)
}

# Functions Negative Binomial - Richards ---------------------------------------------

NBRichGradient <- function(pars, ti, cc, lp)
{
  # r <- exp(pars[1])
  # h <- exp(pars[2])
  # p <- exp(pars[3])
  # s <- exp(pars[4])
  n <- exp(pars[5])
  
  llambdat <- lp(ti = ti, pars = pars[-length(pars)])
  lambdat <- exp(llambdat)
  d1lt <- d1Rich(ti = ti, pars = pars[-length(pars)])
  
  dlr <- d1lt[, 1]
  dlh <- d1lt[, 2]
  dlp <- d1lt[, 3]
  dls <- d1lt[, 4]
  
  dr <- (- n*sum(1/(lambdat+n)*dlr) + sum(exp(log(cc)-llambdat)*dlr) - 
           sum(exp(log(cc)-log(lambdat+n))*dlr))
  dh <-(- n*sum(1/(lambdat+n)*dlh) + sum(exp(log(cc)-llambdat)*dlh) - 
          sum(exp(log(cc)-log(lambdat+n))*dlh))
  dp <- (- n*sum(1/(lambdat+n)*dlp) + sum(exp(log(cc)-llambdat)*dlp) - 
           sum(exp(log(cc)-log(lambdat+n))*dlp))
  ds <- (- n*sum(1/(lambdat+n)*dls) + sum(exp(log(cc)-llambdat)*dls) - 
           sum(exp(log(cc)-log(lambdat+n))*dls))
  dn <- sum(digamma(n+cc) - digamma(n) + log(n) - 
              log(lambdat+n) + (lambdat-cc)/(lambdat+n)) * n
  
  out <- c(dr, dh, dp, ds, dn)
  
  return(out)
}

NBRichHessian <- function(pars, ti, cc, lp)
{
  # r <- exp(pars[1])
  # h <- exp(pars[2])
  # p <- exp(pars[3])
  # s <- exp(pars[4])
  n <- exp(pars[5])
  
  llambdat <- lp(ti = ti, pars = pars[-length(pars)])
  lambdat <- exp(llambdat)
  d1lt <- d1Rich(ti = ti, pars = pars[-length(pars)])
  d2lt <- d2Rich(ti = ti, pars = pars[-length(pars)])
  
  dlr <- d1lt[, 1]
  dlh <- d1lt[, 2]
  dlp <- d1lt[, 3]
  dls <- d1lt[, 4]
  
  dlrr <- dlr + d2lt[, 1]
  dlrh <- d2lt[, 2]
  dlrp <- d2lt[, 3]
  dlrs <- d2lt[, 4]
  dlhh <- dlh + d2lt[, 5]
  dlhp <- d2lt[, 6]
  dlhs <- d2lt[, 7]
  dlpp <- d2lt[, 8]
  dlps <- d2lt[, 9]
  dlss <- dls + d2lt[, 10]
  
  drr <- sum(dlr^2*(exp(log(cc+n)-2*log(lambdat+n))-exp(log(cc)-2*llambdat))) +
    sum(dlrr*(exp(log(cc)-llambdat)-exp(log(cc+n)-log(lambdat+n))))
  
  drh <- sum(dlr*dlh*(exp(log(cc+n)-2*log(lambdat+n))-exp(log(cc)-2*llambdat))) +
    sum(dlrh*(exp(log(cc)-llambdat)-exp(log(cc+n)-log(lambdat+n))))
  drp <- sum(dlr*dlp*(exp(log(cc+n)-2*log(lambdat+n))-exp(log(cc)-2*llambdat))) +
    sum(dlrp*(exp(log(cc)-llambdat)-exp(log(cc+n)-log(lambdat+n))))
  drs <- sum(dlr*dls*(exp(log(cc+n)-2*log(lambdat+n))-exp(log(cc)-2*llambdat))) +
    sum(dlrs*(exp(log(cc)-llambdat)-exp(log(cc+n)-log(lambdat+n))))
  drn <- sum(dlr*(cc-lambdat)/(lambdat+n)^2) * n
  
  dhh <- sum(dlh^2*(exp(log(cc+n)-2*log(lambdat+n))-exp(log(cc)-2*llambdat))) +
    sum(dlhh*(exp(log(cc)-llambdat)-exp(log(cc+n)-log(lambdat+n)))) 
  dhp <- sum(dlh*dlp*(exp(log(cc+n)-2*log(lambdat+n))-exp(log(cc)-2*llambdat))) +
    sum(dlhp*(exp(log(cc)-llambdat)-exp(log(cc+n)-log(lambdat+n))))
  dhs <- sum(dlh*dls*(exp(log(cc+n)-2*log(lambdat+n))-exp(log(cc)-2*llambdat))) +
    sum(dlhs*(exp(log(cc)-llambdat)-exp(log(cc+n)-log(lambdat+n))))
  dhn <- sum(dlh*(cc-lambdat)/(lambdat+n)^2) * n
  
  dpp <- sum(dlp*dlp*(exp(log(cc+n)-2*log(lambdat+n))-exp(log(cc)-2*llambdat))) +
    sum(dlpp*(exp(log(cc)-llambdat)-exp(log(cc+n)-log(lambdat+n)))) 
  dps <- sum(dlp*dls*(exp(log(cc+n)-2*log(lambdat+n))-exp(log(cc)-2*llambdat))) +
    sum(dlps*(exp(log(cc)-llambdat)-exp(log(cc+n)-log(lambdat+n)))) 
  dpn <- sum(dlp*(cc-lambdat)/(lambdat+n)^2)*n
  
  dss <- sum(dls^2*(exp(log(cc+n)-2*log(lambdat+n))-exp(log(cc)-2*llambdat))) +
    sum(dlss*(exp(log(cc)-llambdat)-exp(log(cc+n)-log(lambdat+n)))) 
  dsn <- sum(dls*(cc-lambdat)/(lambdat+n)^2)*n
  
  dn <- sum(digamma(n+cc) - digamma(n) + log(n) - 
              log(lambdat+n) + (lambdat-cc)/(lambdat+n)) * n
  dnn <- (dn + 
            sum(trigamma(cc+n)-trigamma(n)+1/n-1/(lambdat+n)-
                  (lambdat-cc)/(lambdat+n)^2) * n^2)
  
  out <- matrix(c(drr, drh, drp, drs, drn,
                  drh, dhh, dhp, dhs, dhn,
                  drp, dhp, dpp, dps, dpn,
                  drs, dhs, dps, dss, dsn,
                  drn, dhn, dpn, dsn, dnn), ncol=length(pars))
  return(out)
}

# Functions Negative Binomial - Mirrored ---------------------------------------------

NBMRich5Gradient <- function(pars, ti, cc, tk, lp)
{
  # r <- exp(pars[1])
  # h1 <- pars[2]
  # h2 <- pars[3]  
  # s <- exp(pars[4])
  n <- exp(pars[5])
  
  t1 <- ti[ti<tk]
  t2 <- ti[ti>=tk]
  cc1 <- cc[ti<tk]
  cc2 <- cc[ti>=tk]
  llambdat1 <- lp(ti = t1, pars = pars[-length(pars)])
  llambdat2 <- lp(ti = t2, pars = pars[-length(pars)])
  lambdat1 <- exp(llambdat1)
  lambdat2 <- exp(llambdat2)
  d1lt <- d1MRich5(ti=ti, pars=pars[-length(pars)], tk=tk)
  d1lt1 <- d1lt[[1]]
  d1lt2 <- d1lt[[2]]
  
  dl1r <- d1lt1[, 1]
  dl2r <- d1lt2[, 1]
  
  dl1h1 <- d1lt1[, 2]
  dl2h1 <- d1lt2[, 2]
  
  dl1h2 <- d1lt1[, 3]
  dl2h2 <- d1lt2[, 3]
  
  dl1s <- d1lt1[, 4]
  dl2s <- d1lt2[, 4]
  
  dr <- sum(-exp(log(n)-log(lambdat1+n))*dl1r + exp(log(cc1)-llambdat1)*dl1r - 
              exp(log(cc1)-log(lambdat1+n))*dl1r)  + 
    (sum(-exp(log(n)-log(lambdat2+n))*dl2r + exp(log(cc2)-llambdat2)*dl2r - 
           exp(log(cc2)-log(lambdat2+n))*dl2r))
  
  dh1 <- sum(-exp(log(n)-log(lambdat1+n))*dl1h1 + exp(log(cc1)-llambdat1)*dl1h1 - 
               exp(log(cc1)-log(lambdat1+n))*dl1h1)  + 
    (sum(-exp(log(n)-log(lambdat2+n))*dl2h1 + exp(log(cc2)-llambdat2)*dl2h1 - 
           exp(log(cc2)-log(lambdat2+n))*dl2h1))
  
  dh2 <- sum(-exp(log(n)-log(lambdat1+n))*dl1h2 + exp(log(cc1)-llambdat1)*dl1h2 - 
               exp(log(cc1)-log(lambdat1+n))*dl1h2)  + 
    (sum(-exp(log(n)-log(lambdat2+n))*dl2h2 + exp(log(cc2)-llambdat2)*dl2h2 - 
           exp(log(cc2)-log(lambdat2+n))*dl2h2))
  
  ds <- sum(-exp(log(n)-log(lambdat1+n))*dl1s + exp(log(cc1)-llambdat1)*dl1s - 
              exp(log(cc1)-log(lambdat1+n))*dl1s)  + 
    (sum(-exp(log(n)-log(lambdat2+n))*dl2s + exp(log(cc2)-llambdat2)*dl2s - 
           exp(log(cc2)-log(lambdat2+n))*dl2s))
  
  dn <- (sum(digamma(n+cc1) - digamma(n) + log(n) - 
               log(lambdat1+n) + (lambdat1-cc1)/(lambdat1+n)) + 
           sum(digamma(n+cc2) - digamma(n) + log(n) - 
                 log(lambdat2+n) + (lambdat2-cc2)/(lambdat2+n))) * n
  
  out <- c(dr, dh1, dh2, ds, dn)
  
  return(out)
}

NBMRich8Gradient <- function(pars, ti, cc, tk, lp)
{
  r1 <- exp(pars[1])
  h1 <- -exp(pars[2])
  h2 <- -exp(pars[3])  
  s1 <- exp(pars[4])
  s2 <- exp(pars[5])
  p1 <- pars[6]
  p2 <- pars[7]
  n <- exp(pars[8])
  
  r2 <- exp(log(r1)+s2*log(1+10^(h2*p2))-s1*log(1+10^(h1*p1)))
  
  t1 <- ti[ti<tk]
  t2 <- ti[ti>=tk]
  cc1 <- cc[ti<tk]
  cc2 <- cc[ti>=tk]
  llambdat1 <- lp(ti = t1, pars = pars[-length(pars)])
  llambdat2 <- lp(ti = t2, pars = pars[-length(pars)])
  lambdat1 <- exp(llambdat1)
  lambdat2 <- exp(llambdat2)
  d1lt <- d1MRich8(ti=ti, pars=pars[-length(pars)], tk=tk)
  d1lt1 <- d1lt[[1]]
  d1lt2 <- d1lt[[2]]
  
  dl1r1 <- d1lt1[, 1]
  dl2r1 <- d1lt2[, 1]
  
  dl1h1 <- d1lt1[, 2]
  dl2h1 <- d1lt2[, 2]
  
  dl1h2 <- d1lt1[, 3]
  dl2h2 <- d1lt2[, 3]
  
  dl1s1 <- d1lt1[, 4]
  dl2s1 <- d1lt2[, 4]
  
  dl1s2 <- d1lt1[, 5]
  dl2s2 <- d1lt2[, 5]
  
  dl1p1 <- d1lt1[, 6]
  dl2p1 <- d1lt2[, 6]
  
  dl1p2 <- d1lt1[, 7]
  dl2p2 <- d1lt2[, 7]
  
  dr1 <- sum(-exp(log(n)-log(lambdat1+n))*dl1r1 + exp(log(cc1)-llambdat1)*dl1r1 - 
               exp(log(cc1)-log(lambdat1+n))*dl1r1)  +
    (sum(-exp(log(n)-log(lambdat2+n))*dl2r1 + exp(log(cc2)-llambdat2)*dl2r1 - 
           exp(log(cc2)-log(lambdat2+n))*dl2r1))
  
  dh1 <- sum(-exp(log(n)-log(lambdat1+n))*dl1h1 + exp(log(cc1)-llambdat1)*dl1h1 - 
               exp(log(cc1)-log(lambdat1+n))*dl1h1)  + 
    (sum(-exp(log(n)-log(lambdat2+n))*dl2h1 + exp(log(cc2)-llambdat2)*dl2h1 - 
           exp(log(cc2)-log(lambdat2+n))*dl2h1))
  dh2 <- sum(-exp(log(n)-log(lambdat1+n))*dl1h2 + exp(log(cc1)-llambdat1)*dl1h2 - 
               exp(log(cc1)-log(lambdat1+n))*dl1h2)  + 
    (sum(-exp(log(n)-log(lambdat2+n))*dl2h2 + exp(log(cc2)-llambdat2)*dl2h2 - 
           exp(log(cc2)-log(lambdat2+n))*dl2h2))
  
  ds1 <- sum(-exp(log(n)-log(lambdat1+n))*dl1s1 + exp(log(cc1)-llambdat1)*dl1s1 - 
               exp(log(cc1)-log(lambdat1+n))*dl1s1)  + 
    (sum(-exp(log(n)-log(lambdat2+n))*dl2s1 + exp(log(cc2)-llambdat2)*dl2s1 - 
           exp(log(cc2)-log(lambdat2+n))*dl2s1))
  ds2 <- sum(-exp(log(n)-log(lambdat1+n))*dl1s2 + exp(log(cc1)-llambdat1)*dl1s2 - 
               exp(log(cc1)-log(lambdat1+n))*dl1s2)  + 
    (sum(-exp(log(n)-log(lambdat2+n))*dl2s2 + exp(log(cc2)-llambdat2)*dl2s2 - 
           exp(log(cc2)-log(lambdat2+n))*dl2s2))
  
  dp1 <- sum(-exp(log(n)-log(lambdat1+n))*dl1p1 + exp(log(cc1)-llambdat1)*dl1p1 - 
               exp(log(cc1)-log(lambdat1+n))*dl1p1)  + 
    (sum(-exp(log(n)-log(lambdat2+n))*dl2p1 + exp(log(cc2)-llambdat2)*dl2p1 - 
           exp(log(cc2)-log(lambdat2+n))*dl2p1))
  dp2 <- sum(-exp(log(n)-log(lambdat1+n))*dl1p2 + exp(log(cc1)-llambdat1)*dl1p2 - 
               exp(log(cc1)-log(lambdat1+n))*dl1p2)  + 
    (sum(-exp(log(n)-log(lambdat2+n))*dl2p2 + exp(log(cc2)-llambdat2)*dl2p2 - 
           exp(log(cc2)-log(lambdat2+n))*dl2p2))
  
  dn <- (sum(digamma(n+cc1) - digamma(n) + log(n) - 
               log(lambdat1+n) + (lambdat1-cc1)/(lambdat1+n)) + 
           sum(digamma(n+cc2) - digamma(n) + log(n) - 
                 log(lambdat2+n) + (lambdat2-cc2)/(lambdat2+n))) * n
  
  out <- c(dr1, dh1, dh2, ds1, ds2, dp1, dp2, dn)
  
  return(out)
}

# Growth GLM --------------------------------------------------------------

growthGLM <- function(count, ti, tPred=NA,
                      family="Poisson", monotone=TRUE, bFix=0, tk=NA, nmirror=8,
                      maxiter=1e4, runs=500, nBoot=1000)
{
  # set.seed(1234)
  # inits <- NULL
  # count <- cumCounts
  # ti <- ts
  # family <- fam
  # monotone <- mon
  # nmirror <- nmir
  # tk=NA
  # maxiter=2000
  # runs=500
  # bFix <- 0
  # tPred <- 60
  # nBoot <- 1000
  
  tiorig <- ti
  ti <- ti/max(tiorig)
  tk <- tk/max(tiorig)
  tPred <- tPred/max(tiorig)
  
  tDiff <- diff(ti)[1]
  
  # Findove vogliamo prevedere?
  if (is.na(tPred))
  {
    tPred <- max(ti)
  }
  
  # Picking the selected Richards and initial values
  if(monotone) 
  {
    # Initial values for optim
    dd <- density(rep(ti, count), from = 0, to = max(ti), bw = (max(ti)-min(ti))/20,
                  n = length(ti))
    
    np <- tryCatch(as.vector(unlist(nplr(dd$x, dd$y/sum(dd$y),
                                         useLog=F, npars=5)@pars)), 
                   error=function(e) {
                     a <- as.vector(unlist(nplr(ti, convertToProp(count), 
                                                useLog=F,npars=5)@pars))
                     return(a)
                   })
    inits <- c(log((np[2])*sum(count)), log(np[4]), np[3], log(np[5]))
    
    # Linear predictor
    lp <- function(pars, ti)
    {
      return(Richards(pars, ti, b=bFix))
    }
  }
  if(!monotone)
  {
    dd <- density(rep(ti, count), from = 0, to = max(ti), 
                  bw = (max(ti)-min(ti))/20,
                  n=length(ti))
    # Find point of maximum (where to mirror) if not five in input
    if (is.na(tk))
    {
      pMax <- which.max(dd$y)
      tk <- dd$x[pMax]
    }
    pMax <- which.min((dd$x-tk)^2)
    
    # Find initial values
    np1 <- tryCatch(as.vector(unlist(nplr(abs(dd$x[1:pMax]-tk), 
                                          dd$y[1:pMax]/sum(dd$y),
                                          useLog=F, npars=5)@pars)), 
                    error=function(e) {
                      co <- count[1:which.max(count)[1]]
                      a <- as.vector(unlist(nplr(1:length(co), convertToProp(co),
                                                 useLog=F,npars=5)@pars))
                      return(a)
                    })
    if (tk<max(ti))# Solo se la serie ha un picco e poi scende
    {
      np2 <- tryCatch(as.vector(unlist(nplr(abs(dd$x[pMax:length(dd$x)]-dd$x[pMax]),
                                            dd$y[length(dd$x):pMax]/sum(dd$y),
                                            useLog=F, npars=5)@pars)),
                      error=function(e) {
                        co <- count[1:which.max(count)[1]]
                        a <- as.vector(unlist(nplr(1:length(co), convertToProp(co),
                                                   useLog=F,npars=5)@pars))
                        return(a)
                      })# Tratto decrescente
    }else
    {
      np2 <- np1
    }
    np1[3] <- abs(np1[3])
    np2[3] <- abs(np2[3])
    np1[4] <- abs(np1[4])
    np2[4] <- abs(np2[4])
    np1[5] <- abs(np1[5])
    np2[5] <- abs(np2[5])
    inits <- c(log(((np1[2])*sum(count)+(np2[2])*sum(count))/2), # r
               log(np1[4]), log(np2[4]), # Le hill dei due tratti
               log(np1[5]), log(np2[5]), # Le s dei due tratti
               np1[3], np2[3]) # Le p dei due tratti
    # plot((dd$x), dd$y/sum(dd$y)*sum(count))
    # points(dd$x[1:pMax], Richards(c(log((np1[2])*sum(count)), np1[4], log(np1[3]), log(np1[5])), 
    #                               dd$x[1:pMax], 0), col=3)
    # points(ti, mirrRich(inits, ti, tk = tk, npar = 8), col = 2)
    # points(ti, count, col=2)
    
    # Linear predictor
    lp <- function(pars, ti)
    {
      return(mirrRich(pars = pars, ti = ti, tk = tk, npar = nmirror))
    }
  }
  
  # Picking the selected distribution
  if(family=="Poisson") 
  {
    lik <- function(x) likPois(x, ti = ti, cc=count, lp=lp)
    neglik <- function(x) -lik(x)
    
    if (monotone)
    {
      neggr <- function(x) -PoisRichGradient(x, ti=ti, cc=count,
                                             lp=lp)
      neghessfun <- function(x) -PoisRichHessian(x, ti=ti, cc=count,
                                                 lp=lp)
      inits2 <- optim(inits, neglik, gr=neggr, method="BFGS")$par
    }
    if (!monotone)
    {
      if (nmirror==5)
      {
        # Restringendo gli inits nel caso a 5 parametri
        inits <- inits[1:4]
        neggr <- function(x) -PoisMRich5Gradient(x, ti=ti, cc=count, tk=tk, lp=lp)
        neghessfun <- NULL
        inits2 <- optim(inits, neglik, gr = neggr, 
                        method="BFGS")$par
      }
      if (nmirror==8)
      {
        neggr <- function(x) -PoisMRich8Gradient(x, ti=ti, cc=count, tk=tk, lp=lp)
        neghessfun <- NULL
        inits2 <- optim(inits, neglik, gr=neggr, 
                        method="BFGS")$par
      }
      if (!(nmirror%in%c(5, 8)))
      {
        stop("Mirror Richards is only available with 5 pars or 8 pars")
      }
    }
  }
  if(family=="Negative Binomial") 
  {
    lik <- function(x) likNB(x, ti = ti, cc=count, lp=lp)
    neglik <- function(x) -lik(x)
    
    if (monotone)
    {
      inits <- c(inits, log(max(count)/2))
      neggr <- function(x) -NBRichGradient(x, ti=ti, cc=count, lp=lp)
      neghessfun <- function(x) -NBRichHessian(x, ti=ti, cc=count, lp=lp)
      
      inits2 <- optim(inits, neglik, gr=neggr, 
                      method="BFGS")$par
    }
    if (!monotone)
    {
      if (nmirror==5)
      {
        # Restringendo gli inits nel caso a 5 parametri
        inits <- inits[1:4]
        inits <- c(inits, log(max(count)/2))
        neggr <- function(x) -NBMRich5Gradient(x, ti=ti, cc=count, tk=tk,
                                               lp=lp)
        neghessfun <- NULL
        inits2 <- optim(inits, neglik, gr=neggr,
                        method="BFGS")$par
      }
      if (nmirror==8)
      {
        inits <- c(inits, log(max(count)/2))
        neggr <- function(x) -NBMRich8Gradient(x, ti=ti, cc=count, tk=tk,
                                               lp=lp)
        NBMRich8Gradient(inits, ti, count, tk, lp)
        neghessfun <- NULL
        inits2 <- optim(inits, neglik, gr=neggr, 
                        method="BFGS")$par
      }
      if (!(nmirror%in%c(5, 8)))
      {
        stop("Mirror Richards is only available with 5 pars or 8 pars")
      }
    }
    
  }
  if(family!="Negative Binomial" & family!="Poisson") 
  {
    stop("Family must be Negative Binomial or Poisson")
  }
  
  # Optimizing using Genetic algorithm
  rg <- ga("real-valued", function(x) lik(x), 
           lower=rep(-20,length(inits)), upper=rep(20,length(inits)),
           maxiter=maxiter/2, run=runs, optim=TRUE, 
           suggestions=rbind(inits, inits2))
  rg2 <- ga("real-valued", function(x) lik(x),
            lower=rep(-20,length(inits)), upper=rep(20,length(inits)),
            maxiter=maxiter/2, run=runs, optim=TRUE)
  rg3 <- ga("real-valued", function(x)  lik(x),
            lower=rep(-abs(min(rg@solution))*1.5,length(inits)),
            upper=rep(abs(max(rg@solution))*1.5,length(inits)),
            maxiter=maxiter, run=runs, optim=TRUE,
            suggestions=rbind(rg@solution,rg2@solution),
            optimArgs=list(control=list(maxit=1000)))
  
  propPars <- list()
  propPars[[1]] <- nlminb(rg@solution[1,], neglik, gradient = neggr, 
                          hessian = neghessfun)
  propPars[[2]] <- nlminb(inits, neglik, gradient = neggr, 
                          hessian = neghessfun)
  propPars[[3]] <- nlminb(rg3@solution[1,], neglik, gradient = neggr, 
                          hessian = neghessfun)
  
  best <- which.min(c(propPars[[1]]$objective,
                      propPars[[2]]$objective,
                      propPars[[3]]$objective))
  pars <- propPars[[best]]$par
  
  # Computing Hessian
  if (family=="Poisson")
  {
    if (monotone)
    {
      hess <- neghessfun(x=pars)
    }
    if (!monotone)
    {
      if (nmirror==5)
      {
        hess <- jacobian(neggr, pars)
      }
      if (nmirror==8)
      {
        hess <- jacobian(neggr, pars) 
      }
    }
  }
  if (family=="Negative Binomial")
  {
    if (monotone)
    {
      hess <- neghessfun(pars)
    }
    if (!monotone)
    {
      if (nmirror==5)
      {
        hess <- jacobian(neggr, pars) 
      }
      if (nmirror==8)
      {
        # jacobian(neglik, pars)
        # neggr(pars)
        hess <- jacobian(neggr, pars) 
      }
    }
  }
  
  convergence <-  tryCatch(corpcor::is.positive.definite(hess),
                           error=function(e) {
                             return(corpcor::is.positive.definite(hess, 
                                                                  method = "chol"))
                           })
  pLOptimum <- F

  if(!convergence) 
  {
    #hess <- tryCatch(corpcor::make.positive.definite(hess),
    #                 error=function(e) 0)
    pLOptimum <- T
    warning("Information matrix is not positive definite, 
            possible local optimum")
  }
  
  se <- NA
  invHess <- tryCatch(solve(hess), error=function(e) return("error"))
  
  # Linear predictor
  newt <- seq(min(ti), tPred, by=tDiff)
  linPred <- exp(lp(pars, newt))
  nPred <- length(newt)
  newt <- seq(min(ti), tPred+10*tDiff, by=tDiff)
  
  # Storing objects
  curves <- matrix(NA, nrow=nBoot, ncol=length(newt))
  qtL <- rep(NA, nPred)
  qtU <- rep(NA, nPred)
  diffcurves <- matrix(NA, nrow=nBoot, ncol=nPred-1)
  qtdiffL <- rep(NA, nPred-1)
  qtdiffU <- rep(NA, nPred-1)
  
  # Bounds
  if(!is.character(invHess))
  {
    se <- sqrt(abs(diag(invHess)))
    
    if(!isSymmetric(hess))
    {
      hess <- (hess+t(hess))/2
    }
    invFish <- solve(hess)
    invFish <- (invFish+t(invFish))/2
    
    rpars <- rmvnorm(nBoot, pars, invFish)
    rlpmat <- apply(rpars, 1, function(x) exp(lp(x, ti = newt)))

    repl <- rep(F, nBoot)
    if (family=="Poisson")
    {
      curves[, 1] <- rpois(nBoot, rlpmat[1, ])
    }
    if (family=="Negative Binomial")
    {
      curves[, 1] <- rnbinom(nBoot, size=exp(rpars[, length(pars)]),
                             mu=rlpmat[1, ])
    }
    
    for(i in 2:length(newt))
    {
      # diag(invFish) <- se
      if (family=="Poisson")
      {
        curves[,i] <- rpois(nBoot, rlpmat[i, ])
        
        if (monotone==T)
        {
          for(j in i:2)
          {
            repl <- (curves[, j]<curves[, j-1])
            curves[repl, (j-1):i] <- (curves[repl, j]*(i-j+1) +
                                        curves[repl, j-1])/(i-j+2)
          }
        }
      }
      if (family=="Negative Binomial")
      {
        curves[,i] <-  rnbinom(nBoot, size=exp(rpars[, length(pars)]),
                               mu=rlpmat[i, ])
        if (monotone==T)
        {
          for(j in i:2)
          {
            repl <- (curves[,j]<curves[,j-1])
            curves[repl, (j-1):i] <- (curves[repl, j]*(i-j+1) +
                                        curves[repl, j-1])/(i-j+2)
          }
        }
      }
    }
    curves <- curves[, 1:nPred]
    
    diffcurves <- t(apply(curves, 1, diff))
    qtCurves <- apply(curves, 2, quantile, prob=c(0.025, 0.975), na.rm=T)
    qtU <- qtCurves[2,]
    qtL <- qtCurves[1,]
    qtdiffCurves <- apply(diffcurves, 2, quantile, prob=c(0.025, 0.975), na.rm=T)
    qtdiffU <- qtdiffCurves[2,]
    qtdiffL <- qtdiffCurves[1,]
    
    # matplot(rlpmat, type="l")
    # 
    # matplot(t(curves), type="l")
    # 
    # matplot(t(diffcurves), type="l")
    # 
    # plot(exp(lp(pars, newt)), col=1, type="l", ylim=c(min(qtL), max(qtU)))
    # points(ts, count)
    # points(qtU, type="l", col=2)
    # points(qtL, type="l", col=2)
    # 
    # plot(diff(exp(lp(pars, newt))), col=1, type="l", ylim=c(min(qtdiffL),
    #                                                         max(qtdiffU)))
    # points(ts[-1], diff(count))
    # points(qtdiffU, type="l", col=2)
    # points(qtdiffL, type="l", col=2)
    # 
    # matplot(rlpmat, type="l")
    # rlpmatdiff <- apply(rlpmat, 2, diff)
    # matplot(rlpmatdiff, type="l")
  }
  
  return(list(linPred=linPred, low=qtL, up=qtU, 
              lowdiff=qtdiffL, updiff=qtdiffU,
              pars=pars, 
              lik=-propPars[[best]]$objective, 
              R2=cor(count, linPred[1:length(count)])^2, 
              se=se, hessian=hess, 
              optim=propPars[[best]],
              lpFun=lp,
              NoConv=pLOptimum,
              BandsError=convergence))
}