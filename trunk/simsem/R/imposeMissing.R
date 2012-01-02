##Function to impost planned, MAR and MCAR missing on a data set
##inputs: data matrix, percent missing for each type of missing, indices of covariates, other arguments to planned missing
##Output: data set with missing data 

test <- function() {
  
  

  # Imposing Missing with the following arguments produces no missing values
  imposeMissing(data)
  imposeMissing(data,covs=c(1,2))
  imposeMissing(data,pmMCAR=0)
  imposeMissing(data,pmMAR=0)
  imposeMissing(data,nforms=0)

  #Some more usage examples
  imposeMissing(data,covs=c(1,2),pmMCAR=.1)
  imposeMissing(datac,covs=c(20,21),pmMAR=.2)
  imposeMissing(data,nforms=3)
  imposeMissing(data,nforms=3,itemGroups=list(c(1,2,3,4,5),c(6,7,8,9,10),c(11,12,13,14,15),c(16,17,18,19)))
  imposeMissing(datac,covs=c(20,21),nforms=3)
  imposeMissing(data,twoMethod=c(19,.8))
  imposeMissing(datac,covs=c(20,21),pmMCAR=.1,pmMAR=.1,nforms=3)
  
}

imposeMissing <- function(data.mat,covs=NULL,pmMCAR=NULL,pmMAR=NULL,nforms=NULL,itemGroups=NULL,twoMethod=NULL){

 # TRUE values are values to delete
 log.matpl <- planned.missing(dim(data.mat),covs,nforms=nforms,twoMethod=twoMethod,itemGroups=itemGroups)
 data.mat[log.matpl] <- NA
 
 # Impose MAR and MCAR
 log.mat1 <- makeMCAR(dim(data.mat),pmMCAR,covs)
 data.mat[log.mat1] <- NA
 
 log.mat2 <- makeMAR(data.mat,pmMAR,covs)
 data.mat[log.mat2] <- NA

 
 return(data.mat) 
 
}


# Function to impose MAR missing based on two covariates
# Input: raw data, desired percent missing, indices of covariates
# Output: Logical matrix of values to be deleted
makeMAR <- function(data,pm=NULL,covs=NULL)
  {
    dat <- matrix(FALSE,dim(data)[1],dim(data)[2])

    if(!is.null(pm) && !is.null(covs)) {
      len.scale <- dim(data)[2]
    
      mar.pred1 <- covs[1]
      mar.pred2 <- covs[2]

    
    ## Create a vector of small deviations 1/2 the length of your scales
      Y <- runif(len.scale*.5,0,.25*pm)

    ## Bind Y with -Y and add pm to each element
       ## Permute the elements just described to get a vector whose elements will average to pm
      if((len.scale %% 2)== 0){
        Z <- sample(c(Y+pm,-Y+pm), size=len.scale, replace=F)
      } else {
        Z <- sample(c(Y+pm,-Y+pm,pm), size=len.scale, replace=F)
      }
      
    ## fun1 asks the question:
    ## Is the prob. associated with the current value of some covariate (normally dist. attached to the dataset) <= x
       ## x = an element of Z (from above)
      fun1 <- function(x,dat) pnorm(dat,mean(dat),sd(dat)) <= x

    # THIS INDEXING IS GOING TO BE REAL - run it through the interpreter to verify before making changes.
    # Basically, this divides the items (columns) into 2 groups that don't include the covariates
      c <- (1:len.scale)[-covs]
      g1 <- c[1:(length(c)/2)]
      g2 <- c[((ceiling(length(c)/2) + 1 - length(c)%% 2):length(c))]

      
    ## Apply fun1 to half of the variables using one covariate and to the other half using a different covariate
       ## Because Z is m elements long and our covariate is p elements long cbind(R1, R2) will total p x m
      R1 <- sapply(Z[g1],fun1,dat=data[,mar.pred1])
      R2 <- sapply(Z[g2],fun1,dat=data[,mar.pred2])

      dat[,g1] <- R1
      dat[,g2] <- R2
    }
    
    return(dat)
    
}

# Function to make some MCAR missin'
# Input: Data matrix dimensions, desired percent missing, columns of covariates to not have missingness on
# Output: Logical matrix of values to be deleted
makeMCAR <- function(dims,pm=NULL,covs=NULL)
  {
    R <- matrix(FALSE,dims[1],dims[2])

    if(!is.null(pm)){
      ## Fills a pattern matrix (R) of the same dimensions as your data with a bunch of binomially distributed (p=pm) TRUEs or FALSEs
       ## Provides an R matrix with a proportion of ones = proportion missing (all iid binomial)
      R <- matrix(as.logical(rbinom(n=dims[2]*dims[1],size=1,prob=pm)),dims[1],dims[2],byrow=TRUE)
    }

    if (!is.null(covs) ) {
       R[,covs] <- FALSE    
    } 
    
    return(R)
}


# Function to poke holes in the data for planned missing designs. Currently, we default a 3-form design.
# Input: Data Set
# Output: Boolean matrix of values to delete
#
# Right now, function defaults to NULL missingness. If number of forms is specified, items are divided equally and
# grouped sequentially.
# (i.e. columns 1-5 are shared, 6-10 are A, 11-15 are B, and 16-20 are C)

# TODO:
# Warnings for illegal groupings
# Check to see if item groupings are valid? 
planned.missing <- function(dims=c(0,0),nforms=NULL,itemGroups=NULL,twoMethod=NULL, covs=NULL) {
  
  nitems <- dims[2]
  nobs <- dims[1]
  excl <- covs

  log.mat <- matrix(FALSE,ncol=nitems,nrow=nobs)

  if(!is.null(nforms) && nforms != 0) {
    if ( ((!is.null(itemGroups)) && (class(itemGroups) != "list")) ) {
      stop("itemGroups not a list")
    } 

   # groups items into sets of column indices (in the 3 form case, shared/a/b/c)

   if (is.null(itemGroups)) {
     items.in.group <- nitems/(nforms+1)
     itemGroups <- generate.indices(nforms+1,items.in.group,excl)
   }

   # groups observations into sets of row indices. Each set "receives" a different "form"

   obs.in.group <- nobs / (nforms)
   obsGroups <- generate.indices(nforms,obs.in.group,excl=NULL)

   # Create Missing Matrix
     for(i in 1:nforms) {
       log.mat[obsGroups[[i]],itemGroups[[i+1]]] <- TRUE
  
     }
  }
   if (!is.null(twoMethod)) {
     col <- unlist(twoMethod[1])
     percent <- unlist(twoMethod[2])
     toDelete <- 1:((percent)*nobs)
     log.mat[toDelete,col] <- TRUE
   }

  return (log.mat)
}


# Default generation method for item groupings and observation groupings.
# Generates sequential groups of lists of numbers based on the desired number of groups,
# and the number of items in a group, excluding certain indices.
#
# EX: generate.indices(3,4)
# [[1]]
# [1] 1 2 3 4
# [[2]]
# [1] 5 6 7 8
# [[3]]
# [1] 9 10 11 12
generate.indices <- function(ngroups, items.in.group,excl=NULL) {
  
  a <- 1:(ngroups*items.in.group) # set of item indices
  
  if(!is.null(excl)){
    anot <- a[-excl]
  } else { anot <- a}
  
  ipg <- length(anot)/ngroups

  for (i in 1:ngroups) {
    if (i==1) {
      index.list <- list(anot[1:ipg])
    }
    else {
      indices.used <- length(unlist(index.list))
      index.list[[i]] <- anot[(indices.used+1):(ipg*i)]
    }
  }
    
  return(index.list)
}

