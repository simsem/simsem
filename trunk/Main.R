# Assumes starting in trunk
setwd(paste(getwd(),"/DataGeneration",sep="")) 

source("simDist.R")
source("simMatrix.R")
source("simMatrixSet.R")
source("simConstraint.R")
source("matrixSet.R")
source("freeParamSet.R")
source("reducedMatrixSet.R")
source("misspecifiedSet.R")
source("simData.R")
source("simModel.R")
source("simResult.R")
source("subMatrixSet.R")
source("simConstraint.R")

# Generate complete data -

complete.l <- run(simData, 1000)

# Impose missing data
imposeMissing <- function(complete.data.l){

  
# returns missing data matrix
  
} # end imposeMissing


missing.l <- imposeMissing(complete.data.l)

imposeMissing <- function(data, a, b, c)

setClass("missing",
         representation=c(
           a="numeric",
           b="numeric",
           c="numeric")
 )

simMiss <- missing.object(a, b, c)
missing.l <- run(simMiss, complete.data.l)

                     

# Impute missing data
imputeMissing <- function(){
  
} # end imputeMissing

imputed.l <- lapply(missing.l, imputeMissing)

# Analyze

# with complete data and  FIML

complete.results.l <- run(simModel, complete.l)


  #lapply(complete.l, run, simModel)

FIML.results.l <- run(missing.l, simModel)

# with MI

MI.results.l <- runMI(imputed.l, simModel)



# Summarize results


complete.summary <-  result.object(complete.results.l)

FIML.summary <- result.object(FIML.results.l)

MI.summary <- result.object(MI.results.l)
