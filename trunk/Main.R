#Assumes start in the trunk
# This allows us to source our files with more organization

install.packages("simsem_0.0-1.tar.gz", repos=NULL, type="source")
library(simsem)
library(Rmpi)

source("ImposeMissing/plmissing2.R")
source("ImposeMissing/MIsummary.r")

# For safety?
.Last <- function(){ 
  if (is.loaded("mpi_initialize")){ 
    if (mpi.comm.size(1) > 0){ 
      print("Please use mpi.close.Rslaves() to close slaves.") 
      mpi.close.Rslaves() 
      } 
      print("Please use mpi.quit() to quit R") 
      .Call("mpi_finalize") 
    } 
} 


# After we spawn computing minions, we need to send them our instructions
mpi.spawn.Rslaves(nslaves=5)
mpi.bcast.Robj2slave(planned.missing)

# Generate complete data -

build.example.model <- function() {
  factor.loading <- matrix(NA, 4, 2)
  factor.loading[,1] <- 1
  factor.loading[,2] <- 0:3
  LY <- matrix.object(factor.loading)

  factor.mean <- rep(NA, 2)
  factor.mean.starting <- c(5,2)
  AL <- vector.object(factor.mean, factor.mean.starting)

  factor.var <- rep(NA,2)
  factor.var.starting <- c(1, 0.25)
  VPS <- vector.object(factor.var, factor.var.starting)

  factor.cor <- matrix(NA,2,2)
  diag(factor.cor) <- 1
  PS <- sym.matrix.object(factor.cor, 0.5)

  VTE <- vector.object(rep(NA,4),1.2)

  TE <- sym.matrix.object(diag(4))

  TY <- vector.object(rep(0,4))

  LCA.Model <- matrix.CFA.object(LY=LY, PS=PS, VPS=VPS, AL=AL,
                               VTE=VTE, TE=TE, TY=TY)
  return(LCA.Model)
  
}


data.model <- build.example.model()
data.object <- data.object(300, data.model)
sim.data.model <- model.object(data.model)


complete.l <- list()
build.data.sets <- function(model,obs,sets) {
  for(i in 1:sets) { complete.l[[i]] <- run(model,obs) }
  return(complete.l) }

complete.l <- build.data.sets(data.object,100,3)

imposeMissing <- function(data.mat){

 # TRUE values are values to delete
 log.mat <- planned.missing(dim(data.mat))

 # This will work when we've made some more design decisions about percent missing and covariates
 
 log.mat1 <- makeMCARbin(data.mat,.1,dim(data.mat)[2])

 # parms <- list()
 # parms$len.scale <- dim(data.mat)[2]-2 # for 2 covariates
 # parms$mar.pred1 <- "cov1"
 # parms$mar.pred2 <- "cov2"
 # parms$pm <- .1

 # log.mat2 <- makeMAR(data.mat,parms)

 data.mat[log.mat] <- NA
 # data.mat[log.mat1] <- NA
 # data.mat[log.mat2] <- NA
 
 return(data.mat) 
 
} 

missing.l <- lapply(complete.l,imposeMissing)
missing.l <- mpi.applyLB(complete.l,imposeMissing)
# missing.l <- imposeMissing(complete.data.l)


# Impute missing data
# Input: 1 Missing data matrix
# Out: List of imputations
imputeMissing <- function(data.mat,imps){
  # pull out only the imputations
  require(Amelia)
  temp.am <- amelia(data.mat,m=imps)
  return(temp.am$imputations)

} # end imputeMissing

#Impute and run data. Nested lappy statements
#Input: 1 missing data matrices
#Output: results from the dataset, combined with Rubin's Rules
#Output is in the form a a list with parameters, SE, fit, FMI.1, and FMI.2
runMI<- function(data.mat,data.model,imps) {
  #Impute missing data
  imputed.l<-imputeMissing(data.mat,imps)
  
  #Run models on each imputed data set
  #Does this give results from each dataset in the list?
  
  # imputed.results<-result.object(imputed.l[[1]],sim.data.model,10)

  imputed.results <- lapply(imputed.l,result.object,data.model,1)
  comb.results<-MIpool(imputed.results,imps)
  
  return(comb.results)

}





# This is a list of lists. e.g. ,imputed.l[[1]] contains results from 1 simualted data set
# imputed[[1]][[1]] is the parameters estimates from the first data set.
MI.results.l <- lapply(missing.l, runMI,sim.data.model,10)
MI.results.l <- mpi.applyLB(missing.l,runMI,10)

#Take list of imputed results and turn into a format to coerce into results.object
#Data frame of estimates, then SE, then fit
MI.results.param<-matrix(NA,nrow=length(MI.results.l),ncol=length(MI.results.l[[1]][[1]]))
MI.results.se<-matrix(NA,nrow=length(MI.results.l),ncol=length(MI.results.l[[1]][[2]]))
MI.results.fit<-matrix(NA,nrow=length(MI.results.l),ncol=length(MI.results.l[[1]][[3]]))

for(i in 1:length(MI.results.l)){
MI.results.param[i,]<-unlist(MI.results.l[[i]][[1]])
MI.results.se[i,]<-unlist(MI.results.l[[i]][[2]])
MI.results.fit[i,]<-unlist(MI.results.l[[i]][[3]])
}

Result <- new("simResult", Replication=length(MI.results.l), Estimates=as.data.frame(MI.results.param), SE=as.data.frame(MI.results.se), Fit=as.data.frame(MI.results.fit), Convergence = c(0))
Result


mpi.close.Rslaves()
mpi.quit()
