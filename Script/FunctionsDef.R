# Packages ----------------------------------------------------------------

library(MASS)
library(rmutil)
library(GA)
library(nplr)
library(lqmm)
library(numDeriv)
require(tidyverse)
require(magrittr)

# Functions - Richards -----------------------------------------------------

# Linear Predictor monotone
Richards <- function(pars, ti, bFix)
{
  den <- (1 + 10^(pars[2] * (exp(pars[3]) - ti)))^exp(pars[4])
  out <- bFix+exp(pars[1])/den
  
  return(out)
}

# Linear Predictor mirrored
mirrRich <- function(pars, ti, npar=8, tiMax=0, bFix=0) 
{
  b <- exp(pars[1])
  
  if (npar==5)
  {
    r <- exp(pars[2])
    
    hill <- ifelse(ti<tiMax, pars[3], pars[4])
    symm <- exp(pars[5])

    den <- (1+10^(hill*abs(ti-tiMax)))^symm
  }
  if (npar==8)
  {
    r <- exp(pars[2])
    
    hill <- ifelse(ti<tiMax,pars[3],pars[4])
    symm <- exp(ifelse(ti<tiMax,pars[5],pars[6]))
    peak <- exp(ifelse(ti<tiMax,pars[7],pars[8]))

    den <- (1+10^(hill*(peak-abs(ti-tiMax))))^symm
  }
  if (npar==9)
  {
    h1 <- pars[3]
    h2 <- pars[4]
    hill <- ifelse(ti<tiMax, h1, h2)
  
    s1 <- exp(pars[5])
    s2 <- exp(pars[6])
    symm <- ifelse(ti<tiMax, s1, s2)
    
    p1 <- exp(pars[7])
    p2 <- exp(pars[8])
    peak <- ifelse(ti<tiMax, p1, p2)
  
    r1 <- exp(pars[2])
    r2 <- r1*(1+10^(h2*p2))^s2/(1+10^(h1*p1))^s1
    r <- ifelse(ti<tiMax, r1, r2)
  
    den <- (1+10^(hill*((peak-abs(ti-tiMax)))))^symm
  }
  
  out <- b+r/den
  if (!(npar%in%c(5, 8, 9)))
  {
    stop("Mirror Richards is only available with 5 pars, 8 pars or 9 pars with continuity constraint")
  }
  
  out <- b+r/den
  
  return(out)
}

# Functions - Poisson -----------------------------------------------------

# Likelihood
likPois <- function(pars, ti, cc, lp, weight=1) 
{
  linPred <- lp(pars=pars, ti=ti)
  out <- sum(weight*dpois(cc, linPred, log=T))
  
  return(out)
}

# Gradient Richard-Poisson
logPoisGradient <- function(pars, ti, cc, lp)
{
  
  r <- exp(pars[1])
  h <- pars[2]
  p <- exp(pars[3])
  s <- exp(pars[4])
  
  lambdat <- lp(pars=pars, ti=ti)
  
  dlr <- (1+10^(h*(p-ti)))^(-s)*r
  dlh <- -r*s*log(10)*(p-ti)*(1+10^(h*(p-ti)))^(-s-1)*10^(h*(p-ti))
  dlp <- -r*s*log(10)*h*(1+10^(h*(p-ti)))^(-s-1)*10^(h*(p-ti))*p
  dls <- -r*(1+10^(h*(p-ti)))^(-s)*log(1+10^(h*(p-ti)))*s
  
  dr <- - sum(dlr) + sum(cc*1/lambdat*dlr)
  dh <- - sum(dlh) + sum(cc*1/lambdat*dlh)
  dp <- - sum(dlp) + sum(cc*1/lambdat*dlp)
  ds <- - sum(dls) + sum(cc*1/lambdat*dls)
  
  out <- c(dr, dh, dp, ds)
  
  return(out)
}

# Hessian Richard-Poisson
logPoisHessian <- function(pars, ti, cc, lp)
{
  r <- exp(pars[1])
  h <- pars[2]
  p <- exp(pars[3])
  s <- exp(pars[4])
  
  lambdat <- lp(pars = pars, ti = ti)
  aux1 <- 10^(h*(p-ti))
  aux2 <- (1+aux1)
  
  dlr <- (aux2)^(-s)*r
  dlh <- -r*s*log(10)*(p-ti)*(aux2)^(-s-1)*aux1
  dlp <- -r*s*log(10)*h*(aux2)^(-s-1)*10^(h*(p-ti))*p
  dls <- -r*(aux2)^(-s)*log(aux2)*s
  
  dlrr <- dlr+rep(0, length(ti))*r^2
  dlrh <- -s*log(10)*(p-ti)*aux2^(-s-1)*aux1*r
  dlrp <- -s*log(10)*h*aux2^(-s-1)*aux1*r*p
  dlrs <- -aux2^(-s)*log(aux2)*r*s
  
  dlhh <- -r*s*(p-ti)^2*(log(10))^2*aux1*aux2^(-s-1)*
    ((-s-1)*aux2^(-1)*aux1+1)
  dlhp <- -r*s*log(10)*aux1*aux2^(-s-1)*((-s-1)*aux2^(-1)*h*log(10)*aux1*(p-ti)+
                                           h*log(10)*(p-ti)+1)*p
  dlhs <- -r*aux1*(p-ti)*log(10)*aux2^(-s-1)*
    (1-s*log(aux2))*s
  
  dlpp <- dlp-r*s*h^2*(log(10))^2*aux1*aux2^(-s-1)*
    ((-s-1)*aux2^(-1)*aux1+1)*p^2
  dlps <- -r*h*aux1*log(10)*aux2^(-s-1)*
    (1-s*log(aux2))*p*s
  
  dlss <- dls+r*(log(aux2))^2*aux2^(-s)*s^2
  
  drr <- -sum(dlrr)+sum(cc*(-lambdat^(-2)*dlr*dlr+lambdat^(-1)*dlrr))
  drh <- -sum(dlrh)+sum(cc*(-lambdat^(-2)*dlr*dlh+lambdat^(-1)*dlrh))
  drp <- -sum(dlrp)+sum(cc*(-lambdat^(-2)*dlr*dlp+lambdat^(-1)*dlrp))
  drs <- -sum(dlrs)+sum(cc*(-lambdat^(-2)*dlr*dls+lambdat^(-1)*dlrs))
  
  dhh <- -sum(dlhh)+sum(cc*(-lambdat^(-2)*dlh*dlh+lambdat^(-1)*dlhh))
  dhp <- -sum(dlhp)+sum(cc*(-lambdat^(-2)*dlh*dlp+lambdat^(-1)*dlhp))
  dhs <- -sum(dlhs)+sum(cc*(-lambdat^(-2)*dlh*dls+lambdat^(-1)*dlhs))
  
  dpp <- -sum(dlpp)+sum(cc*(-lambdat^(-2)*dlp*dlp+lambdat^(-1)*dlpp))
  dps <- -sum(dlps)+sum(cc*(-lambdat^(-2)*dlp*dls+lambdat^(-1)*dlps))
  
  dss <- -sum(dlss)+sum(cc*(-lambdat^(-2)*dls*dls+lambdat^(-1)*dlss))
  
  out <- matrix(c(drr, drh, drp, drs,
                  drh, dhh, dhp, dhs,
                  drp, dhp, dpp, dps,
                  drs, dhs, dps, dss), ncol=4)
  
  return(out)
}


# Functions Negative Binomial ---------------------------------------------

likNB <- function(pars, ti, cc, lp, weight=1) 
{
  linPred <- lp(pars[-length(pars)], ti)
  out <- sum(weight*dnbinom(cc, 
                            size=exp(pars[length(pars)]), mu=linPred, 
                            log=T))
}


# Growth GLM --------------------------------------------------------------

growthGLM <- function(count, ti, tiMax=NA, 
                       family="Poisson", monotone=TRUE, bFix=0, nmirror=5,
                       maxiter=1e4, runs=500, weight=1) 
{
  
  # Picking the selected Richards and initial values
  if(monotone) 
  {
    # Initial values for optim
    dd <- density(c(rep(ti, count), max(ti)+(max(ti)-rep(ti, count))), 
                  from = 0, to = max(ti), bw = 3)
    countSmooth <- dd$y*sum(count)
    
    p1 <- max(count)-bFix
    np <- as.vector(unlist(nplr(dd$x, dd$y, 
                                useLog=F, npars=5)@pars))
    
    inits <- c(log(p1), np[4], log(np[3]), log(np[5]))
    
    # Linear predictor
    lp <- function(pars, ti)
    {
      return(Richards(pars, ti, b=bFix))
    }
  }
  if(!monotone)
  {
    dd <- density(c(rep(ti, count), max(ti)+(max(ti)-rep(ti, count))), 
                  from = 0, to = max(ti), bw = 3)
    countSmooth <- dd$y*sum(count)
    
    # Find point of maximum (where to mirror) if not five in input
    if (is.na(tiMax))
    {
      pMax <- which.max(dd$y)
      tiMax <- dd$x[pMax]
    }
    
    # Find initial values
    p1 <- min(dd$y) # b
    p2 <- max(countSmooth)-min(countSmooth) # r
    np1 <- as.vector(unlist(nplr(dd$x[1:pMax], dd$y[1:pMax], 
                                 useLog=F, npars=5)@pars))   # Tratto crescente
    if (tiMax<max(ti))# Solo se la serie ha un picco e poi scende
    {
      np2 <- as.vector(unlist(nplr(dd$x[pMax:length(dd$x)]-dd$x[pMax], 
                                 dd$y[length(dd$x):pMax], 
                                 useLog=F, npars=5)@pars))   # Tratto decrescente
    }else
    {
      np2 <- np1
      np2[4] <- -np1[4]
    }
    
    inits <- c(log(p1), log(p2), # b ed r
               np1[4], np2[4], # Le hill dei due tratti
               log(np1[5]), log(np2[5]), # Le s dei due tratti
               log(np1[3]), log(np2[3])) # Le p dei due tratti
    
    # Restringendo gli inits nel caso a 5 parametri
    if (nmirror==5)
    {
      inits <- inits[1:5]
    }
    
    # Linear predictor
    lp <- function(pars, ti)
    {
        return(mirrRich(pars, ti, npar=nmirror, tiMax=tiMax, bFix=0))
    }
  }

  # Picking the selected distribution
  if(family=="Poisson") 
  {
    lik <- likPois
  }
  if(family=="nb") 
  {
    inits <- c(inits, 0)
    lik <- likNB
  }
  if(family!="nb" & family!="Poisson") 
  {
    stop("Family must be nb or Poisson")
  }
  
  # Optimizing using optim
  if (family=="Poisson")
  {
    if (monotone)
    {
      inits2 <- optim(inits, function(x) -lik(x, ti, cc=count, 
                                              lp=lp, weight=weight), 
                      gr=function(x) -logPoisGradient(x, ti=ti, cc=count,
                                                      lp=lp), 
                      method="BFGS")$par
    }
    if (!monotone)
    {
      inits2 <- optim(inits, function(x) -lik(x, ti, cc=count, 
                                              lp=lp, weight=weight), 
                      method="BFGS")$par
    }

  }
  if (family=="nb")
  {
    inits2 <- optim(inits, function(x) -lik(x, ti, cc=count, 
                                            lp=lp, weight=weight), 
                    method="BFGS")$par
  }
  
  # Optimizing using Genetic algorithm
  rg <- ga("real-valued", function(x) lik(x, ti, cc=count, 
                                          lp=lp, weight=weight), 
           lower=rep(-20,length(inits)), upper=rep(20,length(inits)),
           maxiter=maxiter/2, run=runs, optim=TRUE, 
           suggestions=rbind(inits, inits2))
  rg2 <- ga("real-valued", function(x) lik(x, ti, cc = count, 
                                           lp = lp, weight = weight), 
            lower=rep(-20,length(inits)), upper=rep(20,length(inits)), 
            maxiter=maxiter/2, run=runs, optim=TRUE)
  rg <- ga("real-valued", function(x)  lik(x, ti, cc = count, 
                                           lp = lp, weight = weight), 
           lower=rep(-abs(min(rg@solution))*1.5,length(inits)), 
           upper=rep(abs(max(rg@solution))*1.5,length(inits)), 
           maxiter=maxiter, run=runs, optim=TRUE, 
           suggestions=rbind(rg@solution,rg2@solution), 
           optimArgs=list(control=list(maxit=1000)))
  
  pars <- rg@solution[1,]
  
  # Computing Hessian
  if (family=="Poisson")
  {
    if (monotone)
    {
      hess <- -logPoisHessian(pars = pars, ti = ti, cc = count,
                              lp = lp)
    }
    if (!monotone)
    {
      hess <- hessian(function(x) -lik(x, ti, cc=count, 
                                       lp=lp, weight=weight), pars)
    }
  }
  if (family=="nb")
  {
    hess <- hessian(function(x) -lik(x, ti, cc=count, 
                                     lp=lp, weight=weight), pars)
  }
  convergence <- is.positive.definite(hess)
  if(!convergence) 
  {
    hess <- make.positive.definite(hess)
  }
  if(!convergence) 
  {
    warning("Information matrix is not positive definite, possible local optimum")
  }
  
  # Standard errors
  se <- sqrt(diag(solve(hess)))
  
  # Linear predictor
  linPred <- lp(pars, ti)
  
  return(list(linPred=linPred, pars=pars, 
              lik=rg@fitnessValue, R2=cor(count, linPred[1:length(count)])^2, 
              se=se, hessian=hess, rg=rg,
              lpFun=lp))
}



