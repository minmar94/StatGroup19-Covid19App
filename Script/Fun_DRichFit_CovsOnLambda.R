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
source("Script/Funs_DRich.R")
source("Script/Funs_DRichPois_CovsOnLambda.R")
source("Script/Funs_DRichNB_CovsOnLambda.R")

# Growth GLM --------------------------------------------------------------

growthGLM <- function(di, ti, tPred=NA, alpha=0.05,
                      family="Poisson", X,
                      maxiter=1e4, runs=500, nBoot=1000)
{
  # set.seed(1234)
  # family <- fam
  # maxiter <- 2000
  # runs <- 500
  # nBoot <- 1000
  # inits <- NULL
  # di <- y
  # alpha <- 0.05
  
  # Rescaling obs
  tiorig <- ti
  # ti <- ti/max(tiorig)
  # tPred <- tPred/max(tiorig)
  tDiff <- diff(ti)[1]
  
  # Covariates and parameters
  ncovs <- ncol(X)
  
  # Findove vogliamo prevedere?
  if (is.na(tPred))
  {
    tPred <- max(ti)
  }
  
  # Initial values for optim
  smooth <- loess(cumsum(di)~ti, degree = 2, span=0.4)
  
  np <- tryCatch(as.vector(unlist(nplr(smooth$x, smooth$fitted,
                                       useLog=F, npars=5)@pars)), 
                 error=function(e) {
                   a <- as.vector(unlist(nplr(ti, convertToProp(di), 
                                              useLog=F,npars=5)@pars))
                   return(a)
                 })
  
  inits <- c(log(np[4]), np[3], np[5], log(np[2]), rep(0, ncovs-1))
  lb <- c(-20, -20, 0, rep(-Inf, ncovs))
  ub <- c(20, 20, sum(di), rep(Inf, ncovs))
  
  # Picking the selected distribution
  if(family=="Poisson") 
  {
    npars <- length(inits)
    nrich <- npars-ncovs
    
    lik <- function(x) likPois(x, ti = ti, di=di, X=X)
    neglik <- function(x) -lik(x)
    
    neggr <- function(x) -PoisRichGradient(x, ti=ti, di=di, X=X)
    neghessfun <- function(x) -PoisRichHessian(x, ti=ti, di=di, X=X)
  }
  if(family=="Negative Binomial") 
  {
    inits <- c(inits, log(mean(di)))
    lb <- c(lb, 0)
    ub <- c(ub, 15)
    npars <- length(inits)
    nrich <- npars-ncovs-1

    lik <- function(x) likNB(x, ti = ti, di=di, X=X)
    neglik <- function(x) -lik(x)
    
    neggr <- function(x) -NBRichGradient(x, ti=ti, di=di, X=X)
    neghessfun <- function(x) -NBRichHessian(x, ti=ti, di=di, X=X)
  }
  if(family!="Negative Binomial" & family!="Poisson") 
  {
    stop("Family must be Negative Binomial or Poisson")
  }
  
  # First optimization
  inits2 <- optim(inits, neglik, gr=neggr, method="BFGS")$par
  inits3 <- multiStart(rbind(inits, inits2), fn=neglik, gr=neggr, quiet = T,
                       control=list(M=20, trace=F))$par
  
  # Second optimization using Genetic algorithm
  rg <- ga("real-valued", function(x) lik(x), 
           lower=lb, upper=ub,
           maxiter=maxiter/2, run=runs, optim=TRUE, 
           suggestions=rbind(inits, inits3), popSize=100, 
           monitor = F)
  rg2 <- ga("real-valued", function(x) lik(x),
            lower=lb, upper=ub, suggestions=rbind(inits2, inits3),
            maxiter=maxiter/2, run=runs, optim=TRUE, popSize=100,
            monitor = F)
  rgsols <- rbind(rg@solution, rg2@solution)
  rg3 <- ga("real-valued", function(x)  lik(x),
            lower=apply(rgsols, 2, min)-2,
            upper=apply(rgsols, 2, max)+2,
            maxiter=maxiter, run=runs, optim=TRUE, popSize=100,
            suggestions=rgsols,
            optimArgs=list(control=list(maxit=1000)),
            monitor = F)
  
  # Third optimization using multistart
  propPars <- optimr::multistart(parmat = rbind(rgsols,
                                                rg3@solution),
                                 fn = neglik, gr = neggr,
                                 control=list(trace=0))
  
  # Picking the best solution
  best <- which.min(propPars$value)
  pars <- as.numeric(propPars[best, 1:npars])
  richpars <- pars[1:nrich]
  betapars <- pars[(nrich+1):(nrich+ncovs)]
  
  # Point prediction
  newt <- seq(min(ti), tPred, by=tDiff)
  newX1 <- rbind(X, as.matrix(X[rep(nrow(X), length(newt)-length(ti)), ]))
  newX <- rbind(X, as.matrix(X[rep((nrow(X)-6):nrow(X), (length(newt)-length(ti))/6), ]))[1:nrow(newX1),, drop = F]
  linPredDiff <- 1e-10+exp(newX%*%betapars+ldiffRich(richpars, newt))
  linPredCum <- cumsum(linPredDiff)
  
  # Computing Hessian
  hess <- neghessfun(pars)
  if (any(is.nan(hess)))
  {
    hess <- jacobian(neggr, pars)
    if (any(is.nan(hess)))
    {
      hess <- hessian(neglik, pars)
    }
  }
  
  # Verifying positive definiteness
  PD <-  tryCatch(corpcor::is.positive.definite(hess),
                  error=function(e) {
                    return(corpcor::is.positive.definite(hess, 
                                                         method = "chol"))
                  })
  
  if(!PD)
  {
    hess <- as.matrix(nearPD(hess)$mat)
    warning("Information matrix is not positive definite, 
            possible local optimum")
  }
  
  # Symmetrize
  invFish <- solve(hess)
  invFish <- (invFish+t(invFish))/2
  # Standard errors
  se <- sqrt(abs(diag(invFish)))
  
  # Prediction interval
  nPred <- length(newt)
  
  # Simulating parameters
  rpars <- rmvnorm(nBoot, pars, invFish)
  rpars <- rpars[rpars[,3]>=0, ]
  if (family=="Negative Binomial")
  {
    rpars[, npars] <- pars[npars]
    #rpars <- rpars[rpars[,5]>0, ]
  }

  # Simulating trajectories
  rlpmat <- apply(rpars, 1, function(x){
    richpars <- x[1:nrich]
    betapars <- x[(nrich+1):(nrich+ncovs)]
    
    1e-10+exp(newX%*%betapars + ldiffRich(pars=richpars, ti = newt))
  })
  
  # Storing objects for intervals
  # First differences
  diffcurves <- matrix(NA, nrow=nBoot, ncol=length(newt))
  
  # Simulating
  for(i in 1:length(newt))
  {
    if (family=="Poisson")
    {
      diffcurves[,i] <- rpois(nBoot, rlpmat[i, ])
    }
    if (family=="Negative Binomial")
    {
      diffcurves[,i] <- rnbinom(nBoot, 
                                size=exp(rpars[, npars]),
                                mu=rlpmat[i, ])
    }
  }
  
  #Cumulative
  curves <- apply(diffcurves, 1, cumsum)
  
  #Quantiles
  qtCurves <- apply(curves, 1, quantile, 
                    probs=c(alpha/2, 1-alpha/2), na.rm=T)
  qtU <- qtCurves[2,]
  qtL <- qtCurves[1,]
  diffqtCurves <- apply(diffcurves, 2, quantile, 
                        probs=c(alpha/2, 1-alpha/2), na.rm=T)
  diffqtU <- diffqtCurves[2,]
  diffqtL <- diffqtCurves[1,]
  
  # matplot(rlpmat, type="l")
  # matplot(curves, type="l")
  # matplot(t(diffcurves), type="l")
  
  # plot(ti, di, pch=19, cex=0.5)
  # lines(newt, linPredDiff, col=2, lwd=1)
  # lines(newt, diffqtU, col=2, lty=2)
  # lines(newt, diffqtL, col=2, lty=2)
  # plot(ti, cumsum(di), pch=19, cex=0.5)
  # lines(newt, linPredCum, col=2, lwd=1)
  # lines(newt, qtU, col=2, lty=2)
  # lines(newt, qtL, col=2, lty=2)
  
  return(list(pars=pars, optim=propPars[[best]],
              linPredDiff=linPredDiff, linPredCum=linPredCum,
              hessian=hess, asyVar=invFish, se=se,
              rpars=rpars, 
              lowdiff=diffqtL, updiff=diffqtU, lowcum=qtL, upcum=qtU, 
              lik=-propPars$value[best], 
              R2diff=cor(di, linPredDiff[1:length(ti)])^2,
              R2cum=cor(cumsum(di), linPredCum[1:length(ti)])^2,
              NoConv=(!PD)))
}

