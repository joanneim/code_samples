## @knitr gibbsloop
# cd "/Users/Joanneim/Dropbox (MIT)/School/14_384/final/"
#scp -r ps2.R joanneim@eosloan.mit.edu:/pool001/joanneim
#module load sloan/R/CRAN/3.5
#module load sloan/R/3.5.1
#R CMD BATCH ps2.R & 
#install.packages(c("condMVNorm", "mvtnorm"))
rm(list=ls())
library("mvtnorm")
library("condMVNorm")

#Main function
gibbs<-function(block, nD=500, nC=500){
  
  #Input: block=list(c(1), c(2))
  #     : nD= number of draws per chain
  #     : nC= number of chains
  
  #Output: Returns quality measure, and the variance of the last draw across chains
  
  #Mean
  params=unlist(block)
  nP=length(params)
  mumat_0=matrix(c(0,0,0),3,1)
  smat_0=matrix(c(1,.8,.2,.8,1,.5,.2,.5,1),3,3)
  
  mumat=mumat_0[params]
  smat=matrix(0, nP,nP)
  for (i in 1:nP){
    for (j in 1:nP){
      smat[i,j]=smat_0[params[i], params[j]]
    }
  }
  
  #Sigma
  nB=length(block)
  
  #Number of parameters
  nP=length(unlist(block))
  
  #Store, and initialize
  draws=array(0,c(nC,nP,nD))
  
  for (c in 1:nC){
    #Enter the right chain. And then initialize.
    draws[c, 1:nP, 1]=rep(10, nP)
    
    for (d in 2:nD){
      
      #Enter the right chain and draw
      prevdraw=draws[c, 1:nP, d-1]
      
      for (b in 1:nB){
        #take the last draws
        bindex=block[[b]]
        nbindex=c(1:nP)[-bindex]
        
        #Get the conditional distribution
        f=condMVN(mean=mumat, sigma=smat, dependent.ind=bindex, given.ind=nbindex, X.given=prevdraw[-bindex], check.sigma=TRUE)
        
        #Draw from the conditional distribution. Enter into right chain, draw, and site.
        draws[c, bindex, d]=rmvnorm(n=1, mean=f$condMean, sigma=f$condVar)
      }
    }
  }
  
  #Quality measure. 
  #take mean across chains
  #mean by chain
  #dimnames(draws)=c("nC", "nP", "nD")
  x_mu=apply(draws[,1,], 1, mean)
  x_mu2=mean(x_mu)
  q = nD/nC * sum((x_mu - x_mu2)^2)
  
  #Chain variance
  cv=1/(nC-1) * sum((draws[,1, nD] - mean(draws[,1, nD]))^2) 
  
  #Return
  return(list(draws=draws, q=q, cv=cv))
}

ntypes=c(100,500,1000)
#ntypes=c(10,50,100)
r=matrix(0, length(ntypes), 2)
colnames(r)=(c("QM", "Var."))
rownames(r)=ntypes


#Specify blocks so that we always sample x first (that is the first parameter entry is always of x)
block1=list(c(1), c(2))
block2=list(c(1,3), c(2))
block3=list(c(1), c(2,3))
block4=list(c(1), c(2), c(3))

blocktypes=list(block1, block2, block3, block4)

r_short=replicate(length(blocktypes), r, simplify=FALSE)
r_long=replicate(length(blocktypes), replicate(length(ntypes), list()))


for (b in 1:length(blocktypes)){
  for (n in 1:length(ntypes)){
    k=gibbs(block=blocktypes[[b]], nD=ntypes[n], nC=500)
    r_short[[b]][n,1]=k$q
    r_short[[b]][n,2]=k$cv
    
    r_long[[b]][[n]]=k
  }
}


save.image("wrkspace.RData")

