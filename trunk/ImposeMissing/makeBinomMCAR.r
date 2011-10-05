
rm(list=ls(all=T))

parms <- list()
parms$pm <- .30
parms$nobs <- 500
parms$len.scale <- 20

data <- matrix(rnorm(parms$nobs*(parms$len.scale+2),0,1),parms$nobs,(parms$len.scale+2))

dat <- data

makeMCARbin <- function(dat,parms)
  {
    pm <- parms$pm
    len.scale <- parms$len.scale
    
    R <- matrix(rbinom(n=len.scale*dim(dat)[1],size=1,prob=pm),dim(dat)[1],len.scale,byrow=TRUE)

    R2 <- cbind(R,matrix(FALSE,dim(dat)[1],(dim(dat)[2]-dim(R)[2])))

    dat[as.logical(R2)] <- NA

    dat
  }


testOut <- makeMCARbin(dat=data,parms)

sum(is.na(testOut))/(parms$len.scale*dim(testOut)[1])

testOut
