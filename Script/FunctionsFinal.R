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
require(Matrix)
require(mcGlobaloptim)
require(BB)
require(optimr)


# Functions - Richard Monotone-----------------------------------------------------

# Linear Predictor monotone
Richards <- function(pars, ti)
{
  # Pars
  logr <- pars[1]
  h <- exp(pars[2])
  p <- pars[3]
  s <- exp(pars[4])
  
  ll1p10hptInf <- log(1+10^(h*(p-ti)))
  ll1p10hpt <- ifelse(is.infinite(ll1p10hptInf), h*(p-ti)*log(10), ll1p10hptInf)
  
  logden <- s*ll1p10hpt
  lout <- log(1e-10+exp(logr-logden))
  
  return(lout)
}

# Derivate prime
d1Rich <- function(pars, ti)
{
  # Pars
  logr <- pars[1]
  h <- exp(pars[2])
  p <- pars[3]
  s <- exp(pars[4])
  laux1Inf <- log(1+10^(h*(p-ti)))
  laux1 <- ifelse(is.infinite(laux1Inf), h*(p-ti)*log(10), laux1Inf)
  
  # First derivatives
  dlr <- exp(-s*laux1 + logr)
  dlh <- -log(10)*(p-ti)*exp(logr+log(s)+(-s-1)*laux1+
                               (h*(p-ti))*log(10)+log(h))
  dlp <- -log(10)*h*exp(logr+log(s)+(-s-1)*laux1+(h*(p-ti))*log(10))
  dls <- -laux1*exp(logr-s*laux1+log(s))
  
  out <- matrix(c(dlr, dlh, dlp, dls), nrow=length(ti))
  
  return(out)
}

# Derivate seconde
d2Rich <- function(pars, ti)
{
  # Pars
  logr <- pars[1]
  h <- exp(pars[2])
  p <- pars[3]
  s <- exp(pars[4])
  
  # First derivatives
  d1lt <- d1Rich(ti = ti, pars = pars)
  dlr <- d1lt[, 1]
  dlh <- d1lt[, 2]
  dlp <- d1lt[, 3]
  dls <- d1lt[, 4]
  
  # Auxiliary
  aux1 <- (1+10^(h*(p-ti)))
  aux2 <- 10^(h*(p-ti))
  laux2 <- (h*(p-ti))*log(10)
  laux1Inf <- log(1+10^(h*(p-ti)))
  laux1 <- ifelse(is.infinite(laux1Inf), h*(p-ti)*log(10), laux1Inf)
  
  # Second derivatives
  dlrr <- dlr + rep(0, length(ti))
  dlrh <- -log(10)*(p-ti)*exp(log(s)+(-s-1)*log(aux1)+laux2+logr+log(h))
  dlrp <- -log(10)*exp(log(h)+log(s)+(-s-1)*log(aux1)+laux2+logr)
  dlrs <- -laux1*exp(-s*laux1+logr+log(s))
  
  dlhh <- dlh - (p-ti)^2*(log(10))^2*exp(logr+log(s)+laux2+(-s-1)*laux1+2*log(h))*
    (-exp(log(s+1)-laux1+laux2)+1)
  
  dlhp <- -log(10)*exp(logr+log(s)+laux2+(-s-1)*laux1+log(h))*
    (-log(10)*(p-ti)*exp(log(s+1)+log(h)-laux1+laux2)+h*log(10)*(p-ti)+1)
  dlhs <- -log(10)*(p-ti)*exp(logr+laux2+(-s-1)*laux1+log(s)+log(h))*
    (1-s*laux1)
  
  dlpp <- - exp(log(log(10)^2)+2*log(h)+logr+log(s)+laux2+(-s-1)*laux1)*
    (-exp(log(s+1)-laux1+laux2)+1)
  dlps <- -log(10)*exp(log(h)+logr+laux2+(-s-1)*laux1+log(s))*(1-s*laux1)
  
  dlss <- dls + (laux1)^2*exp(logr-s*laux1+2*log(s))
  
  # Output
  out <- matrix(c(dlrr, dlrh, dlrp, dlrs, 
                  dlhh, dlhp, dlhs,
                  dlpp, dlps,
                  dlss), nrow=length(ti))
  
  return(out)
}

# Functions - Richard Mirrored -----------------------------------------------------

# Linear Predictor mirrored
mirrRich <- function(pars, ti, tk) 
{
  logr1 <- pars[1]
  
  h1 <- -exp(pars[2])
  h2 <- -exp(pars[3])
  hill <- ifelse(ti<tk, h1, h2)
  
  s1 <- exp(pars[4])
  s2 <- exp(pars[5])
  symm <- ifelse(ti<tk, s1, s2)
  
  p1 <- pars[6]
  p2 <- pars[7]
  peak <- ifelse(ti<tk, p1, p2)
  
  logr2 <- logr1+s2*log(1+10^(h2*p2))-s1*log(1+10^(h1*p1))
  logr <- ifelse(ti<tk, logr1, logr2)
  
  laux1Inf <- log((1+10^(hill*(peak-abs(ti-tk)))))
  laux1 <- ifelse(is.infinite(laux1Inf), (hill*(peak-abs(ti-tk)))*log(10), laux1Inf)
  logden <- symm*laux1
  
  lout <- log(1e-10 + exp(logr-logden))
  return(lout)
}

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
  
  t1 <- ti[ti<tk]
  t2 <- ti[ti>=tk]
  
  # Auxiliary
  tilde1 <- abs(t1-tk)
  tilde2 <- abs(t2-tk)
  pmt1 <- p1-tilde1
  pmt2 <- p2-tilde2
  l1hp <- 10^(h1*pmt1)
  l2hp <- 10^(h2*pmt2)
  ll1hp <- (h1*pmt1)*log(10)
  ll2hp <- (h2*pmt2)*log(10)
  l1aux <- (1+l1hp)
  l2aux <- (1+l2hp)
  ll1auxInf <- log(l1aux)
  ll2auxInf <- log(l2aux)
  ll1aux <- ifelse(is.infinite(ll1auxInf), ll1hp, ll1auxInf)
  ll2aux <- ifelse(is.infinite(ll2auxInf), ll1hp, ll2auxInf)
  l1p10hp <- 1+10^(h1*p1)
  l2p10hp <- 1+10^(h2*p2)
  ll1p10hp <- log(l1p10hp)
  ll2p10hp <- log(l2p10hp)
  
  dl1r1 <- exp(-s1*ll1aux+logr1)
  dl2r1 <- exp(s2*ll2p10hp-s1*ll1p10hp-s2*ll2aux+logr1)
  
  dl1h1 <- - log(10)*pmt1*h1*exp(logr1+log(s1)+(-s1-1)*ll1aux+ll1hp)
  dl2h1 <- - log(10)*p1*h1*exp(logr1+s2*ll2p10hp-s2*ll2aux +log(s1)+ 
                                 (-s1-1)*ll1p10hp+(h1*p1)*log(10))
  
  dl1h2 <- rep(0, length(t1))
  dl2h2 <- - log(10)*h2*exp(logr1-s1*ll1p10hp+
                              s2*ll2p10hp-s2*ll2aux+log(s2))*
    (exp(-ll2aux+ll2hp)*pmt2-exp(-ll2p10hp+(h2*p2)*log(10))*p2)
  
  dl1s1 <- -ll1aux * exp(logr1 -s1*ll1aux+log(s1))
  dl2s1 <- -ll1p10hp*exp(logr1+s2*ll2p10hp-
                           s2*ll2aux-s1*ll1p10hp+log(s1))
  
  dl1s2 <- rep(0, length(t1))
  dl2s2 <- (ll2p10hp-ll2aux) * 
    exp(logr1-s1*ll1p10hp+s2*ll2p10hp-s2*ll2aux+log(s2))
  
  
  dl1p1 <- -h1*log(10)*exp(logr1+log(s1)+(-s1-1)*ll1aux+ll1hp)
  dl2p1 <- -h1*log(10)*exp(logr1+s2*ll2p10hp-s2*ll2aux+log(s1)+
                             (-s1-1)*ll1p10hp+(h1*p1)*log(10))
  
  dl1p2 <- rep(0, length(t1))
  dl2p2 <- -h2*log(10) * 
    exp(logr1-s1*ll1p10hp+s2*ll2p10hp-s2*ll2aux+log(s2)) *
    (exp(-ll2aux+ll2hp) - exp(-ll2p10hp+(h2*p2)*log(10)))
  
  out1 <- matrix(c(dl1r1, dl1h1, dl1h2, dl1s1, dl1s2, dl1p1, dl1p2), 
                 nrow=length(t1))
  out2 <- matrix(c(dl2r1, dl2h1, dl2h2, dl2s1, dl2s2, dl2p1, dl2p2), 
                 nrow=length(t2))
  
  return(list(out1, out2))
}

d2MRich8 <- function(pars, ti, tk)
{
  logr1 <- pars[1]
  h1 <- - exp(pars[2])
  h2 <- - exp(pars[3])  
  s1 <- exp(pars[4])
  s2 <- exp(pars[5])
  p1 <- pars[6]
  p2 <- pars[7]
  
  t1 <- ti[ti<tk]
  t2 <- ti[ti>=tk]
  
  # First derivs
  d1l <- d1MRich8(pars, ti, tk)
  d1l1 <- d1l[[1]]
  d1l2 <- d1l[[2]]
  
  d1l1r1 <- d1l1[, 1]
  d1l1h1 <- d1l1[, 2]
  d1l1h2 <- d1l1[, 3]
  d1l1s1 <- d1l1[, 4]
  d1l1s2 <- d1l1[, 5]
  d1l1p1 <- d1l1[, 6]
  d1l1p2 <- d1l1[, 7]
  
  d1l2r1 <- d1l2[, 1]
  d1l2h1 <- d1l2[, 2]
  d1l2h2 <- d1l2[, 3]
  d1l2s1 <- d1l2[, 4]
  d1l2s2 <- d1l2[, 5]
  d1l2p1 <- d1l2[, 6]
  d1l2p2 <- d1l2[, 7]
  
  # Auxiliary
  tilde1 <- abs(t1-tk)
  tilde2 <- abs(t2-tk)
  pmt1 <- p1-tilde1
  pmt2 <- p2-tilde2
  l1hp <- 10^(h1*pmt1)
  l2hp <- 10^(h2*pmt2)
  ll1hp <- (h1*pmt1)*log(10)
  ll2hp <- (h2*pmt2)*log(10)
  l1aux <- (1+l1hp)
  l2aux <- (1+l2hp)
  ll1auxInf <- log(l1aux)
  ll2auxInf <- log(l2aux)
  ll1aux <- ifelse(is.infinite(ll1auxInf), ll1hp, ll1auxInf)
  ll2aux <- ifelse(is.infinite(ll2auxInf), ll1hp, ll2auxInf)
  l1p10hp <- 1+10^(h1*p1)
  l2p10hp <- 1+10^(h2*p2)
  ll1p10hp <- log(l1p10hp)
  ll2p10hp <- log(l2p10hp)
  
  
  # Lambda 1, r1
  dl1r1r1 <- d1l1r1 + rep(0, length(t1))
  dl1r1h1 <- -log(10)*h1*pmt1*exp(log(s1)-(s1+1)*ll1aux+ll1hp+
                                    logr1) 
  dl1r1h2 <- rep(0, length(t1)) 
  dl1r1s1 <- -ll1aux*exp(-s1*ll1aux+
                           logr1+log(s1))
  dl1r1s2 <- rep(0, length(t1))
  dl1r1p1 <- -log(10)*h1*exp(log(s1)-(s1+1)*ll1aux+ll1hp+
                               logr1)
  dl1r1p2 <- rep(0, length(t1))
  
  # Lambda 2, r1
  dl2r1r1 <- d1l2r1 + rep(0, length(t2))
  dl2r1h1 <- -log(10)*p1*h1*exp(-s2*ll2aux+s2*ll2p10hp+
                                  log(s1)-(s1+1)*ll1p10hp+h1*p1*log(10)+
                                  logr1) 
  dl2r1h2 <- log(10)*h2*exp(-s1*ll1p10hp+log(s2)+s2*ll2p10hp-
                              s2*ll2aux+h2*p2*log(10)+
                              logr1)  *
    (p2*l2p10hp^(-1)-pmt2*exp(-h2*tilde2*log(10)-ll2aux))
  dl2r1s1 <- -ll1p10hp*exp(-s2*ll2aux+s2*ll2p10hp-
                             s1*ll1p10hp+
                             logr1+log(s1))
  dl2r1s2 <- exp(-s1*ll1p10hp+s2*ll2p10hp-s2*ll2aux+
                   logr1+log(s2))*
    (ll2p10hp-ll2aux)
  dl2r1p1 <- -log(10)*h1*exp(-s2*ll2aux+s2*ll2p10hp+
                               log(s1)-(s1+1)*ll1p10hp+h1*p1*log(10)+
                               logr1)
  dl2r1p2 <- log(10)*h2*exp(-s1*ll1p10hp+log(s2)+s2*ll2p10hp+
                              h2*p2*log(10)-s2*ll2aux+
                              logr1)*
    (l2p10hp^(-1)-exp(-h2*tilde2*log(10)-ll2aux))
  
  # Lambda 1, h1
  dl1h1h1 <- d1l1h1 -(pmt1)^2*(log(10))^2*h1^2*exp(logr1+log(s1)+ll1hp+(-s1-1)*ll1aux)*
    (-exp(log(s1+1)-ll1aux+ll1hp)+1)
  dl1h1h2 <- rep(0, length(t1)) 
  dl1h1s1 <- -log(10)*(pmt1)*h1*exp(logr1+ll1hp+(-s1-1)*ll1aux+
                                      log(s1))  *
    (1-s1*ll1aux)
  dl1h1s2 <- rep(0, length(t1)) 
  dl1h1p1 <- -log(10)*h1*exp(logr1+log(s1)+ll1hp+(-s1-1)*ll1aux)  *
    (-log(10)*pmt1*h1*exp(log(s1+1)-ll1aux+ll1hp)+h1*pmt1*log(10)+1)
  dl1h1p2 <- rep(0, length(t1)) 
  
  # Lambda 2, h1
  dl2h1h1 <- d1l2h1 - log(10)^2*h1^2*p1^2*exp(logr1+log(s1)+s2*ll2p10hp-s2*ll2aux+
                                                h1*p1*log(10)+(-s1-1)*ll1p10hp)*
    (-exp(log(s1+1)-ll1p10hp+h1*p1*log(10))+1)
  dl2h1h2 <- -(log(10))^2*p1*h1*h2*exp(logr1+log(s1)+(-s1-1)*ll1p10hp+
                                         h1*p1*log(10)+log(s2)+(s2-1)*ll2p10hp+
                                         h2*p2*log(10)+
                                         (-s2-1)*ll2aux)  *
    (p2*l2aux-pmt2*exp(ll2p10hp-h2*tilde2*log(10)))
  dl2h1s1 <- -log(10)*p1*exp(logr1+s2*ll2p10hp-s2*ll2aux+h1*p1*log(10)+
                               (-s1-1)*ll1p10hp+
                               log(s1))*h1 *
    (-s1*ll1p10hp+1)
  dl2h1s2 <- -log(10)*p1*h1*exp(logr1+log(s1)+(-s1-1)*ll1p10hp+h1*p1*log(10)+
                                  s2*ll2p10hp-s2*ll2aux+
                                  log(s1))  *
    (ll2p10hp-ll2aux)
  dl2h1p1 <- -log(10)*h1*exp(logr1+log(s1)+s2*ll2p10hp-s2*ll2aux+h1*p1*log(10)+
                               (-s1-1)*ll1p10hp)  *
    (-exp(log(s1+1)-ll1p10hp+h1*p1*log(10))*h1*p1*log(10)+h1*p1*log(10)+1)
  dl2h1p2 <- -p1*log(10)^2*h1*h2*exp(logr1+log(s1)+(-s1-1)*ll1p10hp+
                                       h1*p1*log(10)+h2*p2*log(10)+log(s2)+
                                       s2*ll2p10hp-s2*ll2aux)  *
    (l2p10hp^(-1)-exp(-ll2aux-h2*tilde2*log(10)))
  
  # Lambda 1, h2
  dl1h2h2 <- d1l1h2 + rep(0, length(t1))
  dl1h2s1 <- rep(0, length(t1)) 
  dl1h2s2 <- rep(0, length(t1)) 
  dl1h2p1 <- rep(0, length(t1)) 
  dl1h2p2 <- rep(0, length(t1)) 
  
  # Lambda 2, h2
  dl2h2h2 <- d1l2h2 + h2^2*log(10)^2*exp(logr1-s1*log(1+10^(h1*p1))+s2*log(1+10^(h2*p2))+
                                           h2*p2*log(10)+log(s2)+
                                           -s2*ll2aux)*
    (
      exp(log(p2^2)+log(s2)+h2*p2*log(10)-2*log(1+10^(h2*p2)))+
        exp(log(p2^2)-log(1+10^(h2*p2)))+
        -p2*pmt2*exp(log(s2)-ll2aux+ll2hp-log(1+10^(h2*p2)))+
        -p2*pmt2*exp(log(s2)-ll2aux+ll2hp-log(1+10^(h2*p2)))+
        -p2*pmt2*exp(-h2*tilde2*log(10)-ll2aux)+
        exp(log(pmt2^2)+log(s2)-2*ll2aux+h2*(p2-2*tilde2)*log(10))+
        -exp(log(p2^2)-2*log(1+10^(h2*p2))+h2*p2*log(10))+
        exp(log(pmt2^2)-2*ll2aux+h2*(p2-2*tilde2)*log(10))+
        +pmt2*tilde2*exp(-h2*tilde2*log(10)-ll2aux)
    )
  
  dl2h2s1 <- -log(10)*h2*exp(logr1+log(s2)+h2*p2*log(10)+s2*ll2p10hp-s2*ll2aux+
                               log(s1)+
                               -s1*ll1p10hp)  * ll1p10hp * 
    (p2*l2p10hp^(-1)-pmt2*exp(-h2*tilde2*log(10)-ll2aux))
  dl2h2s2 <- log(10)*h2*exp(logr1-s1*ll1p10hp+h2*p2*log(10)+s2*ll2p10hp-s2*ll2aux+
                              log(s2))  *
    (s2*(ll2p10hp-ll2aux)+1)*
    (p2*l2p10hp^(-1)-pmt2*exp(-h2*tilde2*log(10)-ll2aux))
  dl2h2p1 <- -log(10)^2*h1*h2*exp(logr1+log(s2)+s2*ll2p10hp+h2*p2*log(10)-s2*ll2aux+
                                    log(s1)+(-s1-1)*ll1p10hp+h1*p1*log(10)) *
    (p2*l2p10hp^(-1)-pmt2*exp(-h2*tilde2*log(10)-ll2aux))
  dl2h2p2 <- log(10)^2*h2^2*exp(logr1+log(s2)-s1*log(1+10^(h1*p1))+h2*p2*log(10)+
                                  s2*log(1+10^(h2*p2))-s2*ll2aux)*
    (
      + p2 * exp(log(s2)-2*log(1+10^(h2*p2))+h2*p2*log(10)) +
        - pmt2*exp(log(s2)-log(1+10^(h2*p2))+ll2hp-ll2aux) +
        + p2 * (1+10^(h2*p2))^(-1) +
        - pmt2*exp(-h2*tilde2*log(10)-ll2aux) +
        - p2 * exp(log(s2)-log(1+10^(h2*p2))+ll2hp-ll2aux) +
        + pmt2*exp(log(s2)-2*ll2aux+h2*(p2-2*tilde2)*log(10)) +
        + (h2*log(10)*(1+10^(h2*p2)))^(-1) +
        - p2*exp(h2*p2*log(10)-2*log(1+10^(h2*p2))) +
        - (h2*log(10))^(-1)*exp(-h2*tilde2*log(10)-ll2aux) +
        + pmt2*exp(-2*ll2aux+h2*(p2-2*tilde2)*log(10))
    )
  
  # Lambda 1, s1
  dl1s1s1 <- d1l1s1 + (ll1aux)^2*exp(logr1-s1*ll1aux+
                                       2*log(s1))
  dl1s1s2 <- rep(0, length(t1)) 
  dl1s1p1 <- -log(10)*h1*exp(logr1+ll1hp+(-s1-1)*ll1aux+
                               log(s1))*(1-s1*ll1aux)
  dl1s1p2 <- rep(0, length(t1))
  
  # Lambda 2, s1
  dl2s1s1 <- d1l2s1 + ll1p10hp^2*exp(logr1+s2*ll2p10hp-s2*ll2aux-
                                       s1*ll1p10hp+
                                       2*log(s1))
  dl2s1s2 <- -ll1p10hp*exp(logr1-s1*ll1p10hp+s2*ll2p10hp-s2*ll2aux+
                             log(s1)+log(s2))*
    (ll2p10hp-ll2aux)
  dl2s1p1 <- -log(10)*h1*exp(logr1+s2*ll2p10hp-s2*ll2aux+
                               (-s1-1)*ll1p10hp+h1*p1*log(10)+
                               log(s1)) *
    (-s1*ll1p10hp+1)
  dl2s1p2 <- -log(10)*h2*ll1p10hp*exp(logr1-s1*ll1p10hp+log(s2)+s2*ll2p10hp+
                                        h2*p2*log(10)-s2*ll2aux+
                                        log(s1))*
    (l2p10hp^(-1)-exp(-h2*tilde2*log(10)-ll2aux))
  
  # Lambda 1, s2
  dl1s2s2 <- d1l1s2 + rep(0, length(t1))
  dl1s2p1 <- rep(0, length(t1))
  dl1s2p2 <- rep(0, length(t1))
  
  # Lambda 2, s2
  dl2s2s2 <- d1l2s2 + (ll2p10hp-ll2aux)^2*
    exp(logr1-s1*log(1+10^(h1*p2))+s2*ll2p10hp-s2*ll2aux+
          2*log(s2))
  dl2s2p1 <- -log(10)*h1*exp(logr1+s2*ll2p10hp-s2*ll2aux+
                               log(s1)+(-s1-1)*ll1p10hp+h1*p1*log(10)+
                               log(s2))*
    (ll2p10hp-ll2aux)
  dl2s2p2 <- log(10)*h2*exp(logr1-s1*ll1p10hp+s2*ll2p10hp-
                              s2*ll2aux+h2*p2*log(10)+
                              log(s2))*
    (s2*(l2p10hp^(-1)-exp(-h2*tilde2*log(10)-ll2aux))*(ll2p10hp-ll2aux)+
       l2p10hp^(-1)-exp(-h2*tilde2*log(10)-ll2aux))
  
  # Lambda 1, p1
  dl1p1p1 <- - log(10)^2*h1^2*exp(logr1+log(s1)+ll1hp+(-s1-1)*ll1aux) *
    (-exp(log(s1+1)-ll1aux+ll1hp)+1)
  dl1p1p2 <- rep(0, length(t1))
  
  # Lambda 2, p1
  dl2p1p1 <- - h1^2*log(10)^2*exp(logr1+s2*ll2p10hp-s2*ll2aux+log(s1)+
                                    (-s1-1)*ll1p10hp+h1*p1*log(10))*
    (-exp(log(s1+1)-ll1p10hp+h1*p1*log(10))+1)
  dl2p1p2 <- -log(10)^2*h1*h2*exp(logr1+log(s1)+(-s1-1)*ll1p10hp+h1*p1*log(10)+
                                    log(s2)+s2*ll2p10hp+h2*p2*log(10)+
                                    -s2*ll2aux)*
    (l2p10hp^(-1)-exp(-ll2aux-h2*tilde2*log(10)))
  
  # Lambda 1, p2
  dl1p2p2 <- rep(0, length(t1))
  
  # Lambda 2, p2
  dl2p2p2 <- log(10)^2*h2^2*exp(logr1+log(s2)-s1*ll1p10hp+
                                  (s2-1)*ll2p10hp+h2*p2*log(10)-s2*ll2aux)*
    ((s2-1)*exp(-ll2p10hp+h2*p2*log(10))+1-exp(log(s2)-ll2aux+ll2hp)+
       -exp(log(s2)+ll2hp-ll2aux)-exp(ll2p10hp-h2*tilde2*log(10)-ll2aux)*
       (1-exp(log(s2+1)-ll2aux+ll2hp)))
  
  out1 <- matrix(c(dl1r1r1, dl1r1h1, dl1r1h2, dl1r1s1, dl1r1s2, dl1r1p1, dl1r1p2, 
                   dl1h1h1, dl1h1h2, dl1h1s1, dl1h1s2, dl1h1p1, dl1h1p2, 
                   dl1h2h2, dl1h2s1, dl1h2s2, dl1h2p1, dl1h2p2, 
                   dl1s1s1, dl1s1s2, dl1s1p1, dl1s1p2, 
                   dl1s2s2, dl1s2p1, dl1s2p2, 
                   dl1p1p1, dl1p1p2, 
                   dl1p2p2), 
                 nrow=length(t1))
  out2 <- matrix(c(dl2r1r1, dl2r1h1, dl2r1h2, dl2r1s1, dl2r1s2, dl2r1p1, dl2r1p2, 
                   dl2h1h1, dl2h1h2, dl2h1s1, dl2h1s2, dl2h1p1, dl2h1p2, 
                   dl2h2h2, dl2h2s1, dl2h2s2, dl2h2p1, dl2h2p2, 
                   dl2s1s1, dl2s1s2, dl2s1p1, dl2s1p2, 
                   dl2s2s2, dl2s2p1, dl2s2p2, 
                   dl2p1p1, dl2p1p2, 
                   dl2p2p2), 
                 nrow=length(t2))

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
  npars <- length(pars)
  
  # Linear predictor
  llambdat <- lp(pars=pars, ti=ti)
  lambdat <- exp(llambdat)
  
  # First derivatives
  d1lt <- d1Rich(ti = ti, pars = pars)

  # Gradient computation
  out <- rep(NA, npars)
  for (i in 1:npars)
  {
    out[i] <- - sum(d1lt[, i]) + sum(exp(log(cc)-llambdat)*d1lt[, i])
  }

  return(out)
}

# Hessian Richard-Poisson
PoisRichHessian <- function(pars, ti, cc, lp)
{
  # pars <- inits
  # cc <- count
  npars <- length(pars)
  
  # Linear predictor
  llambdat <- lp(pars = pars, ti = ti)
  aux1 <- exp(log(cc)-llambdat)
  aux2 <- -exp(log(cc)-2*llambdat)
  
  # First derivatives
  d1lt <- d1Rich(ti = ti, pars = pars)

  # Second derivatives
  d2lt <- d2Rich(ti = ti, pars = pars)

  jumped <- c(0,1,3,6,10,15,21,28)
  out <- matrix(NA, npars, npars)
  
  # Hessian computation
  for (i in 1:npars)
  {
    for (j in i:npars)
    {
      idx2 <- (i-1)*npars+j-jumped[i]
      
      out[i, j] <- -sum(d2lt[, idx2])+sum(aux2*d1lt[, i]*d1lt[, j]+
                                            aux1*d2lt[, idx2])
      out[j, i] <- out[i, j]
    }
  }

  return(out)
}


# Functions MirroredRichards-Poisson --------------------------------------

# Gradiente Mirrored-Richard-Poisson
PoisMRich8Gradient <- function(pars, ti, cc, tk, lp)
{
  npars <- length(pars)
  
  # Data
  t1 <- ti[ti<tk]
  t2 <- ti[ti>=tk]
  cc1 <- cc[ti<tk]
  cc2 <- cc[ti>=tk]
  
  # Linear predictor
  llambdat <- lp(pars = pars, ti = ti)
  llambdat1 <- llambdat[ti<tk]
  llambdat2 <- llambdat[ti>=tk]
  aux11 <- log(cc1)-llambdat1
  aux12 <- log(cc2)-llambdat2

  # First derivatives
  d1lt <- d1MRich8(ti=ti, pars=pars, tk=tk)
  d1lt1 <- d1lt[[1]]
  d1lt2 <- d1lt[[2]]

  # Gradient computation
  out <- rep(NA, npars)
  for (i in 1:npars)
  {
    out[i] <- sum(-d1lt1[, i] + exp(aux11)*d1lt1[, i]) + 
      sum(-d1lt2[, i] + exp(aux12)*d1lt2[, i])
  }

  return(out)
}

# Hessian Richard-Poisson
PoisMRich8Hessian <- function(pars, ti, cc, tk, lp)
{
  #cc <- count
  npars <- length(pars)
  
  # Data
  t1 <- ti[ti<tk]
  t2 <- ti[ti>=tk]
  cc1 <- cc[ti<tk]
  cc2 <- cc[ti>=tk]

  # Linear predictor
  llambdat <- lp(pars = pars, ti = ti)
  llambdat1 <- llambdat[ti<tk]
  llambdat2 <- llambdat[ti>=tk]
  aux11 <- exp(log(cc1)-llambdat1)
  aux12 <- exp(log(cc2)-llambdat2)
  aux21 <- -exp(log(cc1)-2*llambdat1)
  aux22 <- -exp(log(cc2)-2*llambdat2)
  
  # First derivatives
  d1lt <- d1MRich8(ti = ti, pars = pars, tk=tk)
  d1lt1 <- d1lt[[1]]
  d1lt2 <- d1lt[[2]]

  # Second derivatives
  d2lt <- d2MRich8(ti = ti, pars = pars, tk=tk)
  d2lt1 <- d2lt[[1]]
  d2lt2 <- d2lt[[2]]
  colSums(d2lt2)
  # Hessian computation
  jumped <- c(0,1,3,6,10,15,21,28)
  out <- matrix(NA, npars, npars)
  for (i in 1:npars)
  {
    for (j in i:npars)
    {
      idx2 <- (i-1)*npars+j-jumped[i]
      
      out[i, j] <- -sum(d2lt1[, idx2])+sum(aux21*d1lt1[, i]*d1lt1[, j]+
                                    aux11*d2lt1[, idx2]) + 
                   -sum(d2lt2[, idx2])+sum(aux22*d1lt2[, i]*d1lt2[, j]+
                                    aux12*d2lt2[,idx2])
      out[j, i] <- out[i, j]
    }
  }
  
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
  # pars <- inits2
  # cc <- count
  npars <- length(pars)
  
  n <- exp(pars[5])
  
  llambdat <- lp(ti = ti, pars = pars[-length(pars)])
  lambdat <- exp(llambdat)
  aux1 <- -exp(log(n+cc)-log(lambdat+n))
  aux2 <- exp(log(cc)-llambdat)

  # First derivatives
  d1lt <- d1Rich(ti = ti, pars = pars[-length(pars)])
  
  # Gradient computation
  out <- rep(NA, npars)
  for (i in 1:(npars-1))
  {
    out[i] <- (sum(aux1*d1lt[, i]) + sum(aux2*d1lt[, i]))
  }
  out[npars] <- sum(digamma(n+cc) - digamma(n) + log(n) - 
              log(lambdat+n) + (lambdat-cc)/(lambdat+n)) * n
  

  return(out)
}

NBRichHessian <- function(pars, ti, cc, lp)
{
  npars <- length(pars)
  n <- exp(pars[5])
  
  llambdat <- lp(ti = ti, pars = pars[-length(pars)])
  lambdat <- exp(llambdat)
  aux1 <- (exp(log(cc+n)-2*log(lambdat+n))-exp(log(cc)-2*llambdat))
  aux2 <- (exp(log(cc)-llambdat)-exp(log(cc+n)-log(lambdat+n)))
  auxn <- (cc-lambdat)/(lambdat+n)^2
  
  # First derivatives
  d1lt <- d1Rich(ti = ti, pars = pars[-length(pars)])
  d2lt <- d2Rich(ti = ti, pars = pars[-length(pars)])
  dn <- sum(digamma(n+cc) - digamma(n) + log(n) - 
              log(lambdat+n) + (lambdat-cc)/(lambdat+n)) * n
  
  jumped <- c(0,1,3,6,10,15,21,28)
  out <- matrix(NA, npars, npars)
  
  # Hessian computation
  for (i in 1:(npars-1))
  {
    for (j in i:(npars))
    {
      if (j==npars)
      {
        
        out[i, j] <- sum(d1lt[, i]*auxn) * n
        out[j, i] <- out[i, j]
        
      }else{
        
        idx2 <- (i-1)*(npars-1)+j-jumped[i]
        
        out[i, j] <- sum(d1lt[, i]*d1lt[, j]*aux1) + sum(d2lt[, idx2]*aux2)
        out[j, i] <- out[i, j]
        
      }
    }
  }
  out[npars, npars] <- (dn + sum(trigamma(cc+n)-trigamma(n)+1/n-1/(lambdat+n)+
                  auxn) * n^2)
  
  return(out)
}

# Functions Negative Binomial - Mirrored ---------------------------------------------

NBMRich8Gradient <- function(pars, ti, cc, tk, lp)
{
  # pars <- inits2
  npars <- length(pars)
  n <- exp(pars[8])
  
  # Data
  t1 <- ti[ti<tk]
  t2 <- ti[ti>=tk]
  cc1 <- cc[ti<tk]
  cc2 <- cc[ti>=tk]
  
  # Linear predictor
  llambdat1 <- lp(ti = t1, pars = pars[-length(pars)])
  llambdat2 <- lp(ti = t2, pars = pars[-length(pars)])
  lambdat1 <- exp(llambdat1)
  lambdat2 <- exp(llambdat2)
  aux11 <- -exp(log(n+cc1)-log(lambdat1+n))
  aux12 <- -exp(log(n+cc2)-log(lambdat2+n))
  aux21 <- exp(log(cc1)-llambdat1)
  aux22 <- exp(log(cc2)-llambdat2)

  # First derivatives
  d1lt <- d1MRich8(ti=ti, pars=pars[-length(pars)], tk=tk)
  d1lt1 <- d1lt[[1]]
  d1lt2 <- d1lt[[2]]
  
  # Gradient computation
  out <- rep(NA, npars)
  for (i in 1:npars-1)
  {
    out[i] <- sum(aux11*d1lt1[, i] + aux21*d1lt1[, i])  +
              sum(aux12*d1lt2[, i] + aux22*d1lt2[, i])
  }
  
  out[npars] <- (sum(digamma(n+cc1) - digamma(n) + log(n) - 
               log(lambdat1+n) + (lambdat1-cc1)/(lambdat1+n)) + 
           sum(digamma(n+cc2) - digamma(n) + log(n) - 
                 log(lambdat2+n) + (lambdat2-cc2)/(lambdat2+n))) * n
  
  return(out)
}

# Hessian Richard-Poisson
NBMRich8Hessian <- function(pars, ti, cc, tk, lp)
{
  npars <- length(pars)
  n <- exp(pars[8])

  # Data
  t1 <- ti[ti<tk]
  t2 <- ti[ti>=tk]
  cc1 <- cc[ti<tk]
  cc2 <- cc[ti>=tk]
  
  # Linear predictor
  llambdat1 <- lp(ti = t1, pars = pars[-length(pars)])
  llambdat2 <- lp(ti = t2, pars = pars[-length(pars)])
  lambdat1 <- exp(llambdat1)
  lambdat2 <- exp(llambdat2)
  aux11 <- (exp(log(cc1+n)-2*log(lambdat1+n))-exp(log(cc1)-2*llambdat1))
  aux21 <- (exp(log(cc1)-llambdat1)-exp(log(cc1+n)-log(lambdat1+n)))
  aux12 <- (exp(log(cc2+n)-2*log(lambdat2+n))-exp(log(cc2)-2*llambdat2))
  aux22 <- (exp(log(cc2)-llambdat2)-exp(log(cc2+n)-log(lambdat2+n)))
  auxn1 <- (cc1-lambdat1)/(lambdat1+n)^2
  auxn2 <- (cc2-lambdat2)/(lambdat2+n)^2
  
  # First derivatives
  d1lt <- d1MRich8(ti=ti, pars=pars[-length(pars)], tk=tk)
  d1lt1 <- d1lt[[1]]
  d1lt2 <- d1lt[[2]]
  dn1 <- sum(digamma(n+cc1) - digamma(n) + log(n) - 
              log(lambdat1+n) + (lambdat1-cc1)/(lambdat1+n)) * n
  dn2 <- sum(digamma(n+cc2) - digamma(n) + log(n) - 
               log(lambdat2+n) + (lambdat2-cc2)/(lambdat2+n)) * n
  
  d2lt <- d2MRich8(ti=ti, pars=pars[-length(pars)], tk=tk)
  d2lt1 <- d2lt[[1]]
  d2lt2 <- d2lt[[2]]
  
  jumped <- c(0,1,3,6,10,15,21,28)
  out <- matrix(NA, npars, npars)
  
  # Hessian computation
  for (i in 1:(npars-1))
  {
    for (j in i:(npars))
    {
      if (j==npars)
      {
        
        out[i, j] <- sum(d1lt1[, i]*auxn1) * n + 
                     sum(d1lt2[, i]*auxn2) * n
        out[j, i] <- out[i, j]
        
      }else{
        
        idx2 <- (i-1)*(npars-1)+j-jumped[i]
        
        out[i, j] <- sum(d1lt1[, i]*d1lt1[, j]*aux11) + sum(d2lt1[, idx2]*aux21) +
                     sum(d1lt2[, i]*d1lt2[, j]*aux12) + sum(d2lt2[, idx2]*aux22)
        out[j, i] <- out[i, j]
        
      }
    }
  }
  out[npars, npars] <- (dn1 + sum(trigamma(cc1+n)-trigamma(n)+1/n-1/(lambdat1+n)+
                                   auxn1) * n^2) + 
                       (dn2 + sum(trigamma(cc2+n)-trigamma(n)+1/n-1/(lambdat2+n)+
                                   auxn2) * n^2)
  
  return(out)
}


# Growth GLM --------------------------------------------------------------

growthGLM <- function(count, ti, tPred=NA,
                      family="Poisson", monotone=TRUE, bFix=0, tk=NA,
                      maxiter=1e4, runs=500, nBoot=1000)
{
  # set.seed(1234)
  # inits <- NULL
  # count <- cumCounts
  # ti <- ts
  # family <- fam
  # monotone <- mon
  # tk=NA
  # maxiter=2000
  # runs=500
  # bFix <- 0
  # tPred <- 60
  # nBoot <- 1000
  
  # Rescaling obs
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
      return(Richards(pars, ti))
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

    # Linear predictor
    lp <- function(pars, ti)
    {
      return(mirrRich(pars = pars, ti = ti, tk = tk))
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
      inits3 <- BBoptim(inits, fn=neglik, gr=neggr, quiet = T,
                        control=list(M=10, trace=F, checkGrad=F))$par
    }
    if (!monotone)
    {
      neggr <- function(x) -PoisMRich8Gradient(x, ti=ti, cc=count, tk=tk, 
                                               lp=lp)
      neghessfun <- function(x) -PoisMRich8Hessian(x, ti=ti, cc=count, tk=tk,
                                                 lp=lp)
      inits2 <- optim(inits, neglik, gr=neggr, 
                        method="BFGS")$par
      inits3 <- BBoptim(inits, fn=neglik, gr=neggr, quiet = T,
                        control=list(M=10, trace=F, checkGrad=F))$par
    }
  }
  if(family=="Negative Binomial") 
  {
    lik <- function(x) likNB(x, ti = ti, cc=count, lp=lp)
    neglik <- function(x) -lik(x)
    
    inits <- c(inits, log(max(count)/2))
    if (monotone)
    {
      neggr <- function(x) -NBRichGradient(x, ti=ti, cc=count, lp=lp)
      neghessfun <- function(x) -NBRichHessian(x, ti=ti, cc=count, lp=lp)
      
      inits2 <- optim(inits, neglik, gr=neggr, 
                      method="BFGS")$par
      inits3 <- BBoptim(inits, fn=neglik, gr=neggr, quiet = T,
                   control=list(M=10, trace=F, checkGrad=F))$par
    }
    if (!monotone)
    {
      neggr <- function(x) -NBMRich8Gradient(x, ti=ti, cc=count, tk=tk,
                                               lp=lp)
      neghessfun <- function(x) -NBMRich8Hessian(x, ti=ti, cc=count, tk=tk, lp=lp)
      inits2 <- optim(inits, neglik, gr=neggr, 
                        method="BFGS")$par
      inits3 <- BBoptim(inits, fn=neglik, gr=neggr, quiet = T,
                        control=list(M=10, trace=F, checkGrad=F))$par
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
           suggestions=rbind(inits, inits2, inits3),
           monitor = F)
  rg2 <- ga("real-valued", function(x) lik(x),
            lower=rep(-20,length(inits)), upper=rep(20,length(inits)),
            maxiter=maxiter/2, run=runs, optim=TRUE,
            monitor = F)
  rg3 <- ga("real-valued", function(x)  lik(x),
            lower=rep(-abs(min(rg@solution))*1.5,length(inits)),
            upper=rep(abs(max(rg@solution))*1.5,length(inits)),
            maxiter=maxiter, run=runs, optim=TRUE,
            suggestions=rbind(rg@solution, rg2@solution),
            optimArgs=list(control=list(maxit=1000)),
            monitor = F)

  # propPars <- list()
  # propPars[[1]] <- nlminb(rg@solution[1,], neglik, gradient = neggr, 
  #                         hessian = neghessfun)
  # propPars[[2]] <- nlminb(inits, neglik, gradient = neggr,
  #                         hessian = neghessfun)
  # propPars[[3]] <- nlminb(rg3@solution[1,], neglik, gradient = neggr, 
  #                         hessian = neghessfun)
  # best <- which.min(c(propPars[[1]]$objective,
  #                     propPars[[2]]$objective,
  #                     propPars[[3]]$objective
  # ))

  # propPars[[1]] <- optim(rg@solution[1,], neglik, gr = neggr,
  #                        method="BFGS")
  # propPars[[2]] <- optim(inits, neglik, gr = neggr,
  #                        method="BFGS")
  # propPars[[3]] <- optim(rg3@solution[1,], neglik, gr = neggr,
  #                        method="BFGS")
  # best <- which.min(c(propPars[[1]]$value,
  #                     propPars[[2]]$value,
  #                     propPars[[3]]$value
  # ))
  # 
  # pars <- propPars[[best]]$par
  
  propPars <- optimr::multistart(rbind(rg3@solution,
                                       inits2,
                                       inits3,
                                       rg2@solution), 
                                 neglik, gr = neggr, method="BFGS", 
                                 control=list(trace=0))
  best <- which.min(propPars$value)
  
  pars <- as.numeric(propPars[best, 1:length(inits)])
  
  # Point prediction
  newt <- seq(min(ti), tPred, by=tDiff)
  linPred <- exp(lp(pars, newt))
  
  # Computing Hessian
  hess <- neghessfun(pars)

  # Verifying positive definiteness
  pLOptimum <- F
  convergence <-  tryCatch(corpcor::is.positive.definite(hess),
                           error=function(e) {
                             return(corpcor::is.positive.definite(hess, 
                                                                  method = "chol"))
                           })
  if(!convergence) 
  {
    pLOptimum <- T
    # hess <- make.positive.definite(hess)
    warning("Information matrix is not positive definite, 
            possible local optimum")
  }
  
  # Verifying invertibility
  se <- NA
  invFish <- tryCatch(solve(hess), error=function(e) {
      newhess <- as.matrix(nearPD(hess)$mat)
      return(solve(newhess))
    })
  
  # Storing objects for intervals
  nPred <- length(newt)
  newt <- seq(min(ti), tPred+10*tDiff, by=tDiff)     # Predict over limit for interval cut
  curves <- matrix(NA, nrow=nBoot, ncol=length(newt))
  qtL <- rep(NA, nPred)
  qtU <- rep(NA, nPred)
  diffcurves <- matrix(NA, nrow=nBoot, ncol=nPred-1)
  qtdiffL <- rep(NA, nPred-1)
  qtdiffU <- rep(NA, nPred-1)
  
  sandVar <- invFish%*%(-neggr(pars))%*%t(-neggr(pars))%*%invFish
  se <- sqrt(abs(diag(sandVar)))
    
  rpars <- rmvnorm(nBoot, pars, sandVar)
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
  
  # diffcurves <- t(apply(curves, 1, diff))
  qtCurves <- apply(curves, 2, quantile, prob=c(0.005, 0.995), na.rm=T)
  qtU <- qtCurves[2,]
  qtL <- qtCurves[1,]
  # qtdiffCurves <- apply(diffcurves, 2, quantile, prob=c(0.025, 0.975), na.rm=T)
  # qtdiffU <- qtdiffCurves[2,]
  # qtdiffL <- qtdiffCurves[1,]
  
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

  return(list(linPred=linPred, low=qtL, up=qtU, 
              #lowdiff=qtdiffL, updiff=qtdiffU,
              pars=pars, 
              lik=-propPars$value[best], 
              R2=cor(count, linPred[1:length(count)])^2, 
              se=se, hessian=hess, 
              optim=propPars[[best]],
              lpFun=lp,
              NoConv=pLOptimum,
              BandsError=convergence))
}