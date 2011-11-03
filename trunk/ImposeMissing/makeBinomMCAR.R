
testMCARbin <- function() {

 data <- matrix(rep(rnorm(10,1,1),20),ncol=20)

 testOut <- makeMCARbin(dim(data),.5,c(1,2))
 
 
 sum(testOut)/(20*10)
 
}

# Function to make some MCAR missin'
# Input: Data matrix dimensions, desired percent missing, columns of covariates to not have missingness on
# Output: Logical matrix of values to be deleted

makeMCAR <- function(dims,pm,covs)
  {
    ## Fills a pattern matrix (R) of the same dimensions as your data with a bunch of binomially distributed (p=pm) TRUEs or FALSEs
       ## Provides an R matrix with a proportion of ones = proportion missing (all iid binomial)
    R <- matrix(as.logical(rbinom(n=dims[2]*dims[1],size=1,prob=pm)),dims[1],dims[2],byrow=TRUE)

    # Preserve covariates
    R[,covs] <- FALSE
    
    return(R)
  }







