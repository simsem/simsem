
testMCARbin <- function() {
 rm(list=ls(all=T))

 parms <- list()
 parms$pm <- .30
 parms$nobs <- 500
 parms$len.scale <- 20

 data <- matrix(rnorm(parms$nobs*(parms$len.scale+2),0,1),parms$nobs,(parms$len.scale+2))

 dat <- data
 testOut <- makeMCARbin(dat=data,parms$pm,parms$len.scale)
 return(testOut)
}

# Function to make some MCAR missin'
# Input: Data, desired percent missing,length of the scale (total variables - any covariates)  
# Output: Logical matrix of values to be deleted

makeMCARbin <- function(dat,pm,len.scale)
  {
    ## Fills a pattern matrix (R) of the same dimensions as your data with a bunch of binomially distributed (p=pm) ones and zeros
       ## Provides an R matrix with a proportion of ones = proportion missing (all iid binomial)
    R <- matrix(rbinom(n=len.scale*dim(dat)[1],size=1,prob=pm),dim(dat)[1],len.scale,byrow=TRUE)

    ## Bind the above matrix to a block of FALSEs to keep from deleting any covariate data
    R2 <- cbind(R,matrix(FALSE,dim(dat)[1],(dim(dat)[2]-dim(R)[2])))

    return(as.logical(R2))
    # dat[as.logical(R2)] <- NA
  }




# sum(is.na(testOut))/(parms$len.scale*dim(testOut)[1])


