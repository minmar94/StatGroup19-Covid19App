# Packages ----------------------------------------------------------------

library(MASS)
library(rmutil)
library(numDeriv)
require(tidyverse)
require(magrittr)
source("Script/Funs_DRich.R")

# Functions Negative Binomial ---------------------------------------------

# Likelihood
likNB <- function(pars, ti, di, X) 
{
  npars <- length(pars)
  ncovs <- ncol(X)
  nrich <- npars-ncovs-1
  richpars <- pars[1:nrich]
  betapars <- pars[(nrich+1):(npars-1)]
  n <- exp(pars[npars])
  
  linPred <- 1e-10+exp(X%*%betapars + ldiffRich(richpars, ti))
  
  out <- sum(dnbinom(di, size=n, mu=linPred, 
                     log=T))
  
  return(out)
}

# Functions Negative Binomial - Richards ---------------------------------------------

NBRichGradient <- function(pars, ti, di, X)
{
  # Parameters
  npars <- length(pars)
  ncovs <- ncol(X)
  nrich <- npars-ncovs-1
  richpars <- pars[1:nrich]
  betapars <- pars[(nrich+1):(npars-1)]
  n <- exp(pars[npars])
  
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
  d1mut <- cbind(c(eXb)*d1lt, c(lambdat)*d1eXb)
  d1n <- (log(n) - digamma(n) + digamma(n+di) - log(mut+n) + (mut-di)/(mut+n))*n

  # Auxiliary
  aux1 <- exp(log(n+di)-log(mut+n))
  aux2 <- exp(log(di)-lmut)
  
  # Gradient computation
  out <- c(colSums(-c(aux1)*d1mut+c(aux2)*d1mut), sum(d1n))
  
  return(out)
}


NBRichHessian <- function(pars, ti, di, X)
{
  # Parameters
  npars <- length(pars)
  ncovs <- ncol(X)
  nrich <- npars-ncovs-1
  richpars <- pars[1:nrich]
  betapars <- pars[(nrich+1):(npars-1)]
  n <- exp(pars[npars])
  
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
  d1mut <- cbind(c(eXb)*d1lt, c(lambdat)*d1eXb)
  d1n <- (log(n) - digamma(n) + digamma(n+di) - log(mut+n) + (mut-di)/(mut+n))*n
  
  # Second derivatives
  d2lt <- d2diffRich(ti = ti, pars = richpars)
  d2eXb <- list()
  for(i in 1:ncovs)
  {
    d2eXb[[i]] <- d1eXb[,i]*as.matrix(X[,i:ncovs])
  }
  d2n <- d1n + (1/n-trigamma(n)+trigamma(n+di)-1/(mut+n)-(mut-di)/(mut+n)^2)*n^2
  
  # Auxiliary
  aux1 <- exp(log(di)-lmut)-exp(log(di+n)-log(mut+n))
  aux2 <- exp(log(di+n)-2*log(mut+n))-exp(log(di)-2*lmut)
  auxnb <- (di-mut)*exp(-2*log(n+mut)+log(n))
  
  # Hessian computation
  out <- matrix(NA, npars, npars)
  
  # Hessian computation
  for (i in 1:npars)
  {
    if (i<=nrich)
    {
      out[i, i:(npars-1)] <- c(colSums(c(aux1*eXb)*d2lt[[i]]+c(aux2*eXb^2)*d1lt[,i]*d1lt[,i:nrich]), 
                           colSums(c(aux1)*d1lt[,i]*d1eXb+c(aux2*exp(llambdat+Xb))*d1lt[,i]*d1eXb))
      out[i:(npars-1), i] <- out[i, i:(npars-1)]
    }
    if (i>nrich & i<npars)
    {
      k <- i-nrich
      
      out[i, i:(npars-1)] <- c(colSums(c(aux1*lambdat)*d2eXb[[k]]+c(aux2*lambdat^2)*d1eXb[,k]*d1eXb[,k:ncovs]))
      out[i:(npars-1), i] <- out[i, i:(npars-1)]
    }
    if (i==npars)
    {
      out[i, -npars] <- colSums(c(auxnb)*d1mut)
      out[-npars, i] <- colSums(c(auxnb)*d1mut)
      out[npars, npars] <- sum(d2n)
    }
  }
  
  return(out)
}

