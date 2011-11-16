##Function to impost planned, MAR and MCAR missing on a data set
##inputs: data matrix, percent missing for each type of missing, indices of covariates, other arguments to planned missing
##Output: data set with missing data 


imposeMissing <- function(data.mat,covs,pmMCAR=0,pmMAR=0,nforms=NULL,itemGroups=NULL,twoMethod=NULL){

 # TRUE values are values to delete
 if(!is.null(nforms) || !is.null(twoMethod)){ 
 log.matpl <- planned.missing(dim(data.mat),covs,nforms=nforms,twoMethod=twoMethod,itemGroups=itemGroups)
  data.mat[log.matpl] <- NA
 }

 # Impose MAR and MCAR
 
 log.mat1 <- makeMCAR(dim(data.mat),pmMCAR,covs)
 
 log.mat2 <- makeMAR(data.mat,pmMAR,covs)


  data.mat[log.mat1] <- NA
  data.mat[log.mat2] <- NA
 
 return(data.mat) 
 
} 

##Problems to be fixed:
## Currently requires one variable to be a covariate can we make covariates not specified (for planned and MCAR)
## Can't get two method to work...


imposeMissing(data,covs=1,pmMCAR=.1,pmMAR=.2,nforms=2)