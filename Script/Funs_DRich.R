# Packages ----------------------------------------------------------------

library(MASS)
require(tidyverse)
require(magrittr)


# Functions - Richards -----------------------------------------------------

# Linear Predictor
lRich <- function(pars, ti)
{
  # Parameters
  logh <- pars[1]
  h <- exp(logh)
  p <- pars[2]
  s <- pars[3]
  
  # Denominator
  l1p10hptInf <- log(1+10^(h*(p-ti)))
  l1p10hpt <- ifelse(is.infinite(l1p10hptInf), h*(p-ti)*log(10), l1p10hptInf)
  logden <- -s*l1p10hpt
  
  # Out
  lout <- logden
  
  return(lout)
}

# Linear Predictor monotone
ldiffRich <- function(pars, ti)
{
  # Parameters
  logh <- pars[1]
  h <- exp(logh)
  p <- pars[2]
  s <- pars[3]
  
  # All times
  tAll <- c(ti[1]-1, ti)
  
  # Auxiliary
  l1p10hptInf <- log(1+10^(h*(p-tAll)))
  l1p10hpt <- ifelse(is.infinite(l1p10hptInf), h*(p-tAll)*log(10), l1p10hptInf)
  
  # Terms
  # 1
  laux1 <- log(diff(exp( -s * l1p10hpt)))
  
  # Out  
  lout <- laux1
  
  return(lout)
}

# Derivate prime
d1diffRich <- function(pars, ti)
{
  # Parameters
  logh <- pars[1]
  h <- exp(logh)
  p <- pars[2]
  s <- pars[3]
  
  # All times
  tAll <- c(ti[1]-1, ti)
  
  # Auxiliary
  l1p10hptInf <- log(1+10^(h*(p-tAll)))
  l1p10hpt <- ifelse(is.infinite(l1p10hptInf), h*(p-tAll)*log(10), l1p10hptInf)
  l10hpt <- h*(p-tAll)*log(10)
  
  # Terms
  #1
  laux1 <- log(diff(exp( -s * l1p10hpt)))
  
  # First derivatives
  
  # h
  dlh <- -s * exp(log(log(10)) + 
                    logh) *               # Jacobian
    diff((p-tAll) * exp(l10hpt - (s+1) * l1p10hpt))
  
  # p
  dlp <- -s * exp(log(log(10)) + logh) *
    diff(exp(l10hpt - (s+1)*l1p10hpt))
  
  # s  
  dls <- -diff(exp(log(l1p10hpt) - s*l1p10hpt))
  
  out <- matrix(c(dlh, dlp, dls), nrow=length(ti), ncol=3)
  
  return(out)
}

# Derivate seconde
d2diffRich <- function(pars, ti)
{
  # Parameters
  logh <- pars[1]
  h <- exp(logh)
  p <- pars[2]
  s <- pars[3]
  
  # All times
  tAll <- c(ti[1]-1, ti)
  
  # First derivatives
  d1lt <- d1diffRich(ti = ti, pars = pars)
  dlh <- d1lt[, 1]
  dlp <- d1lt[, 2]
  dls <- d1lt[, 3]
  
  # Auxiliary
  l1p10hptInf <- log(1+10^(h*(p-tAll)))
  l1p10hpt <- ifelse(is.infinite(l1p10hptInf), h*(p-tAll)*log(10), l1p10hptInf)
  l10hpt <- h*(p-tAll)*log(10)
  
  # Terms
  #1
  laux1 <- log(diff(exp( -s * l1p10hpt)))
  
  # Second derivatives
  # h
  dlhh <- dlh + -exp(log(s)+2*log(log(10)) + 
                       2*logh) *                      # Jacobian
    diff( exp(log((p-tAll)^2) + l10hpt + (-s-1)*l1p10hpt) * 
            (1 - exp(log(s+1) - l1p10hpt + l10hpt)) )
  dlhp <- -exp(log(s)+log(log(10)) + 
                 logh) *                      # Jacobian
    diff( exp(l10hpt + (-s-1)*l1p10hpt) * 
            (1 + (p-tAll)*h*log(10)*
               (1 - exp(log(s+1) - l1p10hpt + l10hpt))) )
  dlhs <- - exp(log(log(10)) + 
                  logh) *                       # Jacobian
    diff( (p-tAll)*exp(l10hpt + (-s-1) * l1p10hpt) * (1-exp(log(s)+log(l1p10hpt))))
  
  dlpp <- - exp(log(s) + 2*log(log(10)) + 2*log(h)) *
    diff( exp((-s-1)*l1p10hpt + l10hpt) * (1-exp(log(s+1)+l10hpt-l1p10hpt)))
  dlps <- - exp(log(log(10)) + logh) *
    diff( exp(l10hpt + (-s-1) * l1p10hpt) * (1-exp(log(s)+log(l1p10hpt))))
  
  dlss <- diff(exp(2*log(l1p10hpt) - s * l1p10hpt))
  
  # Output
  out <- list(matrix(c(dlhh, dlhp, dlhs), nrow=length(ti), ncol=3),
              matrix(c(dlpp, dlps), nrow=length(ti), ncol=2),
              matrix(c(dlss), nrow=length(ti), ncol=1))
  
  return(out)
}