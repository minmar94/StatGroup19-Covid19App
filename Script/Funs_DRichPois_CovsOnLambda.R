# Packages ----------------------------------------------------------------

library(MASS)
library(rmutil)
library(numDeriv)
require(tidyverse)
require(magrittr)
source("Script/Funs_DRich.R")

# Functions - Poisson -----------------------------------------------------

# Likelihood
likPois <- function(pars, ti, di, X) 
{
  npars <- length(pars)
  ncovs <- ncol(X)
  richpars <- pars[1:(npars-ncovs)]
  betapars <- pars[(npars-ncovs+1):npars]
  
  linPred <- 1e-10+exp(X%*%betapars + ldiffRich(richpars, ti))
  
  out <- sum(dpois(di, linPred, log=T))
  
  return(out)
}

# Gradient Richard-Poisson
PoisRichGradient <- function(pars, ti, di, X)
{
  # Parameters
  npars <- length(pars)
  ncovs <- ncol(X)
  nrich <- npars-ncovs
  richpars <- pars[1:nrich]
  betapars <- pars[(nrich+1):npars]
  
  # Linear predictor
  lambdat <- exp(ldiffRich(richpars, ti))
  llambdat <- log(lambdat)
  Xb <- X%*%betapars
  eXb <- exp(Xb)
  mut <- 1e-10+exp(Xb+llambdat)
  lmut <- log(mut)
  
  # First derivatives
  d1lt <- d1diffRich(pars, ti)
  d1eXb <- c(eXb)*X
  d1mut <- cbind(c(eXb)*d1lt, c(lambdat)*d1eXb)
  
  # Auxiliary
  aux1 <- exp(log(di)-lmut)
  
  # Gradient computation
  out <- colSums(c(aux1-1)*d1mut)

  return(out)
}

# Hessian Richard-Poisson
PoisRichHessian <- function(pars, ti, di, X)
{
  # Parameters
  npars <- length(pars)
  ncovs <- ncol(X)
  nrich <- npars-ncovs
  richpars <- pars[1:nrich]
  betapars <- pars[(nrich+1):npars]
  
  # Linear predictor
  lambdat <- exp(ldiffRich(richpars, ti))
  llambdat <- log(lambdat)
  Xb <- X%*%betapars
  eXb <- exp(Xb)
  mut <- 1e-10+exp(Xb+llambdat)
  lmut <- log(mut)
  
  # First derivatives
  d1lt <- d1diffRich(richpars, ti)
  d1eXb <- c(eXb)*X
  
  # Second derivatives
  d2lt <- d2diffRich(richpars, ti)
  d2eXb <- list()
  for(i in 1:ncovs)
  {
    d2eXb[[i]] <- d1eXb[,i]*as.matrix(X[,i:ncovs])
  }

  # Auxiliary
  aux1 <- (di-mut)/mut
  aux2 <- exp(log(di)-2*lmut)
  
  jumped <- c(0,1,3,6,10,15,21,28)
  out <- matrix(NA, npars, npars)
  
  # Hessian computation
  for (i in 1:npars)
  {
    if (i<=nrich)
    {
      out[i, i:npars] <- c(colSums(c(aux1*eXb)*d2lt[[i]]-c(aux2*eXb^2)*d1lt[,i]*d1lt[,i:nrich]), 
                    colSums(c(aux1)*d1lt[,i]*d1eXb-c(aux2*mut)*d1lt[,i]*d1eXb))
      out[i:npars, i] <- out[i, i:npars]
    }
    if (i>nrich)
    {
      k <- i-nrich
      
      out[i, i:npars] <- colSums(c(aux1*lambdat)*d2eXb[[k]]-c(aux2*lambdat^2)*d1eXb[,k]*d1eXb[,k:ncovs])
      out[i:npars, i] <- out[i, i:npars]
    }
  }
  
  return(out)
}
