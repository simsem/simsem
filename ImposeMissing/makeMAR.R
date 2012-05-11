
# Test of the makeMar function
testMAR <- function () {
 
 len.scale <- 6
 mar.pred1 <- "dat1"
 mar.pred2 <- "dat2"
 pm <- .15
 nobs <- 500

 data <- cbind(matrix(rnorm(len.scale*nobs,10,15),nobs,len.scale),as.matrix(rnorm(nobs,2,1)),as.matrix(rnorm(nobs,0,1)))
  
 miss.test <- makeMAR(data,.15,c(8,9))
 miss.null <- makeMAR(data)
 
 sum(miss.test)/(nobs*len.scale)

 return(miss.test)
 
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

