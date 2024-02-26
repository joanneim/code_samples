rm(list=ls())

xmin=-5
xmax=5
nG=100

x.points <- seq(xmin, xmax, length.out=nG)
y.points <- x.points

z <- matrix(0, nrow=nG, ncol=nG)
zb <- matrix(0, nrow=nG, ncol=nG)

#Two normals
mu1 <- c(1.5,1.5)
mu1b <- mu1*2
sigma1 <- matrix(c(1,.5,.5,1), 2,2)

mu2 <- -1*mu1
mu2b <- mu2*2
sigma2 <- sigma1

for (i in 1:nG){
  for (j in 1:nG){
    z[i,j] <- .5* dmvnorm(c(x.points[i], y.points[j]), mean=mu1, sigma=sigma1)+ .5* dmvnorm(c(x.points[i], y.points[j]), mean=mu2, sigma=sigma2)
    zb[i,j] <- .5* dmvnorm(c(x.points[i], y.points[j]), mean=mu1b, sigma=sigma1)+ .5* dmvnorm(c(x.points[i], y.points[j]), mean=mu2b, sigma=sigma2)
  }
}


#######

f1<-function(theta, mu1, mu2){
  pdf=.5 * dmvnorm(theta, mean=mu1, sigma=sigma1, log = FALSE)+.5 * dmvnorm(theta, mean=mu2, sigma=sigma2,log = FALSE)
  
  return(pdf)
}

mcmc<-function(tau, mu1, mu2){
  #Draws. Rows=draw number. 
  nD=2000
  draws=matrix(0,nD,2)
  reject=matrix(0,nD,1)
  
  #Initialize draws
  draws[1,1]=10
  draws[1,2]=-10
  reject[1,1]=0
  
  #Sigma of normal draws
  drawSigma = tau * diag(1,2,2)
  
  #Run MC
  for (i in 2:nD){
    y=rmvnorm(1, mean=draws[i-1,], sigma=drawSigma)
    
    #on y
    piy=f1(y, mu1=mu1, mu2=mu2)
    
    #on x
    pix=f1(draws[i-1,], mu1=mu1, mu2=mu2)
    
    #y to x
    qyx=dmvnorm(draws[i-1,], mean=y, sigma=drawSigma, log = FALSE)
    
    #x to y
    qxy=dmvnorm(y, mean=draws[i-1,], sigma=drawSigma, log = FALSE)
    
    #frac
    cutoff=(piy*qyx)/(pix*qxy)
    axy = min(1, cutoff)
    
    #draw uniform
    udraw=runif(1)
    
    if (udraw<axy){
      draws[i,]=y
    }
    else{
      draws[i,]=draws[i-1,]
      reject[i,1]=1
    }
  }
  return(list(draws=draws, reject=reject))
}
mcmcplot<-function(x){
  draws=x$draws
  nD=dim(draws)[1]
  
  cmean=apply(draws,2, function(x) TTR::runMean(x, cumulative=TRUE))
  cvar=apply(draws,2, function(x) TTR::runSD(x, cumulative=TRUE)^2)
  
  rr=sum(x$reject)/nD
  
  return(list(cmean=cmean, cvar=cvar, rr=rr))
}

save.image("contouroutput.RData")