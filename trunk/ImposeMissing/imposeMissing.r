##Function to impost planned, MAR and MCAR missing on a data set
##inputs: data matrix, percent missing for each type of missing, indices of covariates, other arguments to planned missing
##Output: data set with missing data 

test <- function() {
  data <- matrix(rep(rnorm(10,1,1),19),ncol=19)
  datac <- cbind(data,rnorm(10,0,1),rnorm(10,5,5))

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
