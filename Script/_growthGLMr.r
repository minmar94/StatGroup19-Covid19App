library(MASS)
library(rmutil)
library(GA)
library(nplr)
library(lqmm)
library(numDeriv) 

Richards=function(ti,pars) {
den=(1+10^(pars[3]*(exp(pars[4])-ti)))^exp(pars[5])
exp(pars[1])+(exp(pars[2]))/den}     

mirrorRichardsr=function(ti,pars) {
symm=exp(pars[4])
hill=ifelse(ti<0,pars[3],pars[5])
den=(1+10^(-hill*ti))^symm
exp(pars[1])+exp(pars[2])/den}

growthGLMr=function(count,ti,timax=NA,family="Poisson",maxiter=1e4,runs=500,monotone=TRUE,weight=1) {

    if(is.na(timax)) {timax=max(ti)}
    ti=ti/timax
    tinew=seq(0,1,length=timax)
      tiorig=ti
    if(!monotone) {
        tinew=(tinew-ti[which.max(count)[1]])
        ti=ti-ti[which.max(count)]
         ti[ti<0]=-(ti[ti<0]^2)
        ti[ti>0]=ti[ti>0]^2
        tinew[tinew<0]=-(tinew[tinew<0]^2)
                tinew[tinew>0]=(tinew[tinew>0]^2)
    }

    p2=log(max(count)-min(count))
    if(!monotone) {
        co=count[1:which.max(count)[1]]
        np=nplr(1:length(co),convertToProp(co),useLog=F,npars=5)@pars}
    if(monotone) {
        np=nplr(tiorig,convertToProp(count),useLog=F,npars=5)@pars}
    np=as.vector(unlist(np))
    inits=c(log(min(count)+1),p2,np[4],log(abs(np[3]+1e-12)),log(np[5]))
    if(!monotone) {inits[4]=inits[3]}
    
    if(family=="nb") {inits=c(inits,0)}

    if(monotone) {lp=Richards}
    if(!monotone) {lp=mirrorRichardsr}

    likPois=function(pars,ti,count) {
        linPred=lp(ti,pars)
        -sum(weight*dpois(count,linPred,log=T))
    }

    likNB=function(pars,ti,count) {
        linPred=lp(ti,pars)
        -sum(weight*dnbinom(count,size=exp(pars[length(pars)]),mu=linPred,log=T))
}

    if(family=="nb") {lik=likNB}
    if(family=="Poisson") {lik=likPois}
    if(family!="nb" & family!="Poisson") {stop("Family must be nb or Poisson")}
    
    inits2=optim(inits,function(x) lik(x,ti=ti,count=count),method="BFGS")$par
    
    rg=ga("real-valued",function(x) -lik(x,ti=ti,count=count),lower=rep(-20,length(inits)),upper=rep(20,length(inits)),maxiter=maxiter/2,run=runs,optim=TRUE,suggestions=rbind(inits,inits2))
    
    rg2=ga("real-valued",function(x) -lik(x,ti=ti,count=count),lower=rep(-20,length(inits)),upper=rep(20,length(inits)),maxiter=maxiter/2,optim=TRUE,run=runs)
    
    rg=ga("real-valued",function(x) -lik(x,ti=ti,count=count),lower=rep(min(rg@solution)*1.5,length(inits)),upper=rep(max(rg@solution)*1.5,length(inits)),maxiter=maxiter,optim=TRUE,suggestions=rbind(rg@solution,rg2@solution),optimArgs=list(control=list(maxit=1000)),run=runs)

    pars=rg@solution[1,]

    hess=hessian(function(x) lik(x,ti=ti,count=count), pars)

    convergence=is.positive.definite(hess)
    if(!convergence) {hess=make.positive.definite(hess)}
    se=rep(NA,length(pars))
    if(!convergence) {warning("Information matrix is not positive definite, possible local optimum")}
    se=sqrt(diag(solve(hess)))

    linPred=lp(tinew,pars)

    return(list(linPred=linPred,pars=pars,lik=rg@fitnessValue,R2=cor(count,linPred[1:length(count)])^2,se=se,hessian=hess,rg=rg))}


