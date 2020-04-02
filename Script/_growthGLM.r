library(MASS)
library(rmutil)

Richards=function(ti,pars) {
den=(1+10^(pars[3]*(pars[4]-ti)))^pars[5]
pars[1]+(exp(pars[2]))/den}

              

mirrorRichards=function(ti,pars,allTi,tmax) {

    cutoff=min(allTi)+diff(range(allTi))*exp(pars[6])/(1+exp(pars[6]))
# MR(t) = R(t,b1,T,h1,p1,s1) I(t< c) + I(t >= c) T-R(t-c,b2,T,h2,p2,s2)
    if(ti<cutoff)
    {
        return(Richards(ti,pars[1:5]))
    }
    if(ti>=cutoff) {
        Top=pars[1]+exp(pars[2])
        rg=Top-exp(pars[7])
        return(Top-Richards(tmax-ti,c(rg,pars[7:10])))
    }
    
}


noncumGLMGamma=function(count,ti,timax=NA,family="Poisson",nstart=5000,useLog=F) {

     lti=ti
    if(useLog) {lti=log10(ti)}

    if(is.na(timax)) {timax=max(ti)}

    wmax=which.max(count)

    p2=log(max(count)-min(count))
    pe2=lti[(wmax+1):length(lti)]-lti[wmax]
    inits=rnorm(3)
    
    if(family=="nb") {inits=c(inits,1)}

    likPois=function(pars,lti,count) {
  if(pars[1]+exp(pars[2])-pars[7]<0) {return(1e30)}
        linPred=dggamma(lti, exp(pars[1]), exp(pars[2]), exp(pars[3]), log=FALSE)
        -sum(dpois(count,linPred,log=T))
    }

    likNB=function(pars,lti,count) {
         if(        pars[1]+exp(pars[2])-pars[7]<0 | pars[6]<min(ti) | pars[6]>max(ti)) {return(1e30)}
        pars=exp(pars)
                linPred=dggamma(lti,pars[1],pars[2],pars[3])
        -sum(dnbinom(count,size=pars[4],mu=linPred,log=T))
}

    if(family=="nb") {lik=likNB}
    if(family=="Poisson") {lik=likPois}
    if(family!="nb" & family!="Poisson") {stop("Family must be nb or Poisson")}

    op=try(optim(inits,function(x) lik(x,lti=lti,count=count),method="BFGS",hessian=F))
    tries=0
    while(inherits(op,"try-error") & tries<50) {
        inits=inits+rnorm(length(inits))
        op=try(optim(inits,function(x) lik(x,lti=lti,count=count),method="BFGS",hessian=F),silent=TRUE)
        tries=tries+1}
    if(tries==50) {stop("Something wrong")}
    if(nstart>1) {
        for(repl in 2:nstart) {
            inits=inits+rnorm(length(inits))
            jnk=try(optim(inits,function(x) lik(x,lti=lti,count=count),method="BFGS",hessian=F),silent=TRUE)
            if(!inherits(jnk,"try-error") && jnk$value<op$value && !any(is.nan(jnk$se))) {op=jnk}
        }

    }
    
    pars=exp(op$par)
    lti.add=(max(ti)+1):timax
    if(useLog) {lti.add=log10(lti.add)}
    lti=c(lti,lti.add)
    linPred=dggamma(lti,pars[1],pars[2],pars[3])
    
    return(list(linPred=linPred,pars=pars,lik=-op$value,R2=cor(count,linPred[1:length(ti)])^2,op=op))}

noncumGLMRichards=function(count,ti,timax=NA,family="Poisson",nstart=100,useLog=F,fixedBottom=FALSE) {

     lti=ti
    if(useLog) {lti=log10(ti)}

    if(is.na(timax)) {timax=max(ti)}

    wmax=which.max(count)

    p2=log(max(count)-min(count))
    pe2=lti[(wmax+1):length(lti)]-lti[wmax]
    
    inits=c(min(count),p2,0.5,mean(lti[1:wmax]),1,0.5,p2,0.5,mean(pe2),1)
    
    if(family=="nb") {inits=c(inits,1)}

    likPois=function(pars,lti,count) {
        linPred=sapply(lti,mirrorRichards,pars=pars,allTi=lti,tmax=timax)
        linPred[linPred==0]=1e-9
        -sum(dpois(count,linPred,log=T))
    }

    likNB=function(pars,lti,count) {
        linPred=sapply(lti,mirrorRichards,pars=pars,allTi=lti,tmax=timax)
                linPred[linPred==0]=1e-9
        -sum(dnbinom(count,size=exp(pars[11]),mu=linPred,log=T))
}

    if(family=="nb") {lik=likNB}
    if(family=="Poisson") {lik=likPois}
    if(family!="nb" & family!="Poisson") {stop("Family must be nb or Poisson")}

    if(fixedBottom && family=="nb") {
        inits=inits[-1]
        lik=function(x,lti,count) {
            pars=c(0,x)
            likNB(pars,lti,count)}
    }
    if(fixedBottom && family=="Poisson") {
        inits=inits[-1]
        lik=function(x,lti,count) {
            pars=c(0,x)
            likPois(pars,lti,count)}
    }
 op=ga("real-valued",function(x) -lik(x,lti,count),lower=rep(-15,10),upper=rep(15,10),maxiter=1000,optim=TRUE,suggestions=op$par)

    op=try(optim(inits,function(x) lik(x,lti=lti,count=count),method="BFGS",hessian=F))
    tries=0
    while(inherits(op,"try-error") & tries<50) {
        inits=inits+rnorm(length(inits))
        op=try(optim(inits,function(x) lik(x,lti=lti,count=count),method="BFGS",hessian=F),silent=TRUE)
        tries=tries+1}
    if(tries==50) {stop("Something wrong")}
    if(nstart>1) {
        for(repl in 2:nstart) {
            inits=inits+rnorm(length(inits))
            jnk=try(optim(inits,function(x) lik(x,lti=lti,count=count),method="BFGS",hessian=F),silent=TRUE)
            if(!inherits(jnk,"try-error") && jnk$value<op$value && !any(is.nan(jnk$se))) {op=jnk}
        }

    }

    if(family=="Poisson") {
        require(GA)
   sop=ga("real-valued",function(x) -lik(x,lti,count),lower=rep(-15,10),upper=rep(15,10),maxiter=1000,optim=TRUE,suggestions=op$par)
    op$value=-max(sop@fitness)
    op$par=sop@solution}
    
    pars=op$par
    pars[2]=exp(pars[2])+pars[1]
    if(family=="nb") {pars[11]=exp(pars[11])}
    lti.add=(max(ti)+1):timax
    if(useLog) {lti.add=log10(lti.add)}
    lti=c(lti,lti.add)
    linPred=as.vector(unlist(sapply(lti,mirrorRichards,pars=pars,allTi=lti,tmax=timax)))
    
    return(list(linPred=linPred,pars=pars,lik=-op$value,R2=cor(count,linPred[1:length(ti)])^2,peak=ifelse(useLog,10^pars[4],pars[4]),op=op))}



growthGLM=function(count,ti,timax=NA,family="Poisson",nstart=5000,useLog=F,fixedBottom=FALSE) {

    lti=ti
    if(useLog) {lti=log10(ti)}

    if(is.na(timax)) {timax=max(ti)}

    p2=log(max(count)-min(count))
    inits=c(min(count),p2,0.5,mean(lti),1)
    
    if(family=="nb") {inits=c(inits,1)}

    likPois=function(pars,lti,count) {
        den=(1+10^(pars[3]*(pars[4]-lti)))^pars[5]
        linPred=pars[1]+(exp(pars[2]))/den
        -sum(dpois(count,linPred,log=T))
    }

likNB=function(pars,lti,count) {
 den=(1+10^(pars[3]*(pars[4]-lti)))^pars[5]
        linPred=pars[1]+(exp(pars[2]))/den
        -sum(dnbinom(count,size=exp(pars[6]),mu=linPred,log=T))
}

    if(family=="nb") {lik=likNB}
    if(family=="Poisson") {lik=likPois}
    if(family!="nb" & family!="Poisson") {stop("Family must be nb or Poisson")}

    if(fixedBottom && family=="nb") {
        inits=inits[-1]
        lik=function(x,lti,count) {
            pars=c(0,x)
            likNB(pars,lti,count)}
    }
    if(fixedBottom && family=="Poisson") {
        inits=inits[-1]
        lik=function(x,lti,count) {
            pars=c(0,x)
            likPois(pars,lti,count)}
    }

    op=try(optim(inits,function(x) lik(x,lti=lti,count=count),method="BFGS",hessian=TRUE))
    tries=0
    while(inherits(op,"try-error") & tries<50) {
        inits=inits+rnorm(length(inits))
        op=try(optim(inits,function(x) lik(x,lti=lti,count=count),method="BFGS",hessian=TRUE),silent=TRUE)
        tries=tries+1}
    if(tries==50) {stop("Something wrong")}
    if(nstart>1) {
        for(repl in 2:nstart) {
            inits=inits+rnorm(length(inits))
            jnk=try(optim(inits,function(x) lik(x,lti=lti,count=count),method="BFGS",hessian=TRUE),silent=TRUE)
            if(!inherits(jnk,"try-error") && jnk$value<op$value && !any(is.nan(jnk$se))) {op=jnk}
        }

    }

    
    pars=op$par
    pars[2]=exp(pars[2])+pars[1]
    if(family=="nb") {pars[6]=exp(pars[6])}
    lti.add=(max(ti)+1):timax
    if(useLog) {lti.add=log10(lti.add)}
    lti=c(lti,lti.add)
den=(1+10^(pars[3]*(pars[4]-lti)))^pars[5]
    linPred=pars[1]+(pars[2]-pars[1])/den

    return(list(linPred=linPred,pars=pars,lik=-op$value,R2=cor(count,linPred[1:length(ti)])^2,peak=ifelse(useLog,10^pars[4],pars[4]),se.unstransformed=sqrt(diag(solve(op$hessian))),op=op))}

growthGLMcov=function(count,ti,useCov=rep(0,5), cov.bottom=0,cov.top=0,cov.peak=0,cov.slope=0,cov.asy=0,gap.par=F,timax=NA,nstart=5000,useLog=F) {
    
    lti=ti
    if(useLog) {lti=log10(ti)}

    if(is.na(timax)) {timax=max(ti)}

    p2=log(max(count)-min(count))
    inits=c(min(count),p2,0.5,mean(lti),1)
    
    if(any(useCov==1)) {inits=c(inits,rep(0,sum(useCov)))}
    if(gap.par) {inits=c(inits,0)}
    
    lik=function(pars,lti,count) {
        if(length(pars<=5+sum(useCov))) {pars=c(pars,0)}
        s=pars[5]+cov.asy*useCov[5]*pars[5+sum(useCov)]
        la=pars[1]+cov.bottom*useCov[1]*pars[5+useCov[1]]
        gap=pars[2]+cov.top*useCov[2]*pars[5+sum(useCov[1:2])]
        hill=pars[3]+cov.slope*useCov[3]*pars[5+sum(useCov[1:3])]
        peak=pars[4]+cov.peak*useCov[4]*pars[5+sum(useCov[1:4])]
        tim=peak-lti
        wm=which.min(abs(tim))
        if(pars[5+sum(useCov)+1]!=0) {
            pa=pars[5+sum(useCov)+1]
            jnk=tim[abs(tim)<pa]
            tim[abs(tim)<pa]=min(abs(tim))
            tim[tim>=abs(pa)]=tim[tim>=abs(pa)]-max(jnk)
            tim[tim<= -abs(pa)]=tim[tim<= -abs(pa)]-min(jnk)
        }
        den=(1+10^(hill*(tim)))^s
        linPred=la+(exp(gap))/den
        -sum(dpois(count,linPred,log=T))
    }

    op=try(optim(inits,function(x) lik(x,lti=lti,count=count),method="BFGS",hessian=TRUE),silent=TRUE)
    tries=0
    while(inherits(op,"try-error") & tries<50) {
    inits=inits+rnorm(length(inits))
    op=try(optim(inits,function(x) lik(x,lti=lti,count=count),method="BFGS",hessian=TRUE),silent=TRUE)
    tries=tries+1
    }
    if(tries==50) {stop("Something wrong here")}
    if(nstart>1) {
        for(repl in 2:nstart) {
            inits=inits+rnorm(length(inits))
            jnk=try(optim(inits,function(x) lik(x,lti=lti,count=count),method="BFGS",hessian=TRUE),silent=TRUE)
            if(!inherits(jnk,"try-error") && jnk$value<op$value) {op=jnk}
        }

    }

    
    pars=op$par
    if(length(pars<=5+sum(useCov))) {pars=c(pars,0)}
        lti.add=(max(ti)+1):timax
        if(useLog) {lti.add=log10(lti.add)}
    lti=c(lti,lti.add)
    if(useCov[5]) {cov.asy=c(cov.asy,rep(cov.asy[length(cov.asy)],length(lti.add)))}
    if(useCov[1]) {cov.bottom=c(cov.bottom,rep(cov.bottom[length(cov.bottom)],length(lti.add)))}
    if(useCov[2]) {cov.top=c(cov.top,rep(cov.top[length(cov.top)],length(lti.add)))}
    if(useCov[3]) {cov.slope=c(cov.slope,rep(cov.slope[length(cov.slope)],length(lti.add)))}
    if(useCov[4]) {cov.peak=c(cov.peak,rep(cov.peak[length(cov.peak)],length(lti.add)))}

    
        s=pars[5]+cov.asy*useCov[5]*pars[5+sum(useCov)]
        la=pars[1]+cov.bottom*useCov[1]*pars[5+useCov[1]]
        gap=pars[2]+cov.top*useCov[2]*pars[5+sum(useCov[1:2])]
        hill=pars[3]+cov.slope*useCov[3]*pars[5+sum(useCov[1:3])]
        peak=pars[4]+cov.peak*useCov[4]*pars[5+sum(useCov[1:4])]
        tim=peak-lti
        wm=which.min(abs(tim))
        if(pars[5+sum(useCov)+1]!=0) {
            pa=pars[5+sum(useCov)+1]
            jnk=tim[abs(tim)<pa]
            tim[abs(tim)<pa]=min(abs(tim))
            tim[tim>=abs(pa)]=tim[tim>=abs(pa)]-max(jnk)
            tim[tim<= -abs(pa)]=tim[tim<= -abs(pa)]-min(jnk)
        }
        den=(1+10^(hill*(tim)))^s
        linPred=la+(exp(gap))/den

    return(list(linPred=linPred,pars=pars,lik=-op$value,R2=cor(count,linPred[1:length(ti)])^2,peak=ifelse(useLog,10^peak,peak),se.unstransformed=sqrt(diag(solve(op$hessian))),op=op))}

