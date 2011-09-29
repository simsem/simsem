#Assumes start in the trunk
# This allows us to source our files with more organization

source("DataGeneration/simDist.R")
source("DataGeneration/simMatrix.R")
source("DataGeneration/simMatrixSet.R")
source("DataGeneration/simConstraint.R")
source("DataGeneration/matrixSet.R")
source("DataGeneration/freeParamSet.R")
source("DataGeneration/reducedMatrixSet.R")
source("DataGeneration/misspecifiedSet.R")
source("DataGeneration/simData.R")
source("DataGeneration/simModel.R")
source("DataGeneration/simResult.R")
source("DataGeneration/subMatrixSet.R")
source("DataGeneration/simConstraint.R")
source("ImposeMissing/plmissing2.R")


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

# It seems like build.example.model should return a simData that actually has a data set in it, and then the run fuction creates 1000 of such data sets.

data.model <- build.example.model()
data.object <- data.object(300, data.model)
# This is actually just a matrix with 1000 observations
# complete.mat <- run(simData, 1000)

build.data.sets <- function(obs,sets) {
  for(i in 1:10) { complete.l[[sets]] <- run(simData,obs) }
  return(complete.l) }

imposeMissing <- function(data.mat){
  
  poke.holes(data.mat)
   
} 

missing.l <- lapply(complete.l,imposeMissing)

# missing.l <- imposeMissing(complete.data.l)

# imposeMissing <- function(data, a, b, c)

#setClass("missing",
#         representation=c(
#           a="numeric",
#           b="numeric",
#           c="numeric")
# )

#simMiss <- missing.object(a, b, c)
#missing.l <- run(simMiss, complete.data.l)

                     

# Impute missing data
imputeMissing <- function(data.mat,imps){
  require(Amelia)
  return(amelia(data.mat,m=imps))
  
} # end imputeMissing

imputed.l <- lapply(missing.l, imputeMissing,10)

# Analyze

# with complete data and  FIML
complete.results.l <- list()
# results <- function(data.model,complete.l) {
#  for(i in 1:length(complete.l)) { results.l[[i]] <- run(data.model,complete.l[[i]]) }
#  return(results.l) }
      



  #lapply(complete.l, run, simModel)

FMIL.results.l <- run(missing.l, simModel)

# with MI

MI.results.l <- runMI(imputed.l, simModel)



# Summarize results


complete.summary <-  result.object(complete.results.l)

FIML.summary <- result.object(FIML.results.l)

MI.summary <- result.object(MI.results.l)


