setwd("/nfs/home/patr1ckm/simsem/trunk/")

install.packages("simsem_0.0-4.tar.gz", repos=NULL, type="source")
library(simsem)
library(debug)

source('AllClass.R')
source("AllGenerics.R")
source('vectorize.object-methods.R')
source( "make.labels-methods.R" )
source("is.null.object-methods.R")
source("simModel-methods.R")
source("run-methods.R")
source("runLavaan.R")
source("blank.parameters.R" )
source("write.lavaan.code.R" )
source("write.lavaan.constraint.R" )
source("collapse.exo.R")
source("extract.lavaan.summary.R" )
source("combine.object-methods.R")

# Generate complete data -

build.example.model <- function() {
  factor.loading <- matrix(NA, 4, 2)
  factor.loading[,1] <- 1
  factor.loading[,2] <- 0:3
  LY <- simMatrix(factor.loading)

  factor.mean <- rep(NA, 2)
  factor.mean.starting <- c(5,2)
  AL <- simVector(factor.mean, factor.mean.starting)

  factor.var <- rep(NA,2)
  factor.var.starting <- c(1, 0.25)
  VPS <- simVector(factor.var, factor.var.starting)

  factor.cor <- matrix(NA,2,2)
  diag(factor.cor) <- 1
  PS <- symMatrix(factor.cor, 0.5)

  VTE <- simVector(rep(NA,4),1.2)

  TE <- symMatrix(diag(4))

  TY <- simVector(rep(0,4))

  LCA.Model <- simSetCFA(LY=LY, PS=PS, VPS=VPS, AL=AL,
                               VTE=VTE, TE=TE, TY=TY)
  return(LCA.Model)
  
}




data.model <- build.example.model()
data.object <- simData(300, data.model)
sim.data.model <- simModel(data.model) # this one is the actual model


complete.l <- list()
build.data.sets <- function(model,obs,sets) {
  for(i in 1:sets) { complete.l[[i]] <- run(model,obs) }
  return(complete.l) }

complete.l <- build.data.sets(data.object,100,3)


missing.l <- lapply(complete.l,imposeMissing,nforms=3)
missing.l <- mpi.applyLB(complete.l,imposeMissing)

#results <- runMI(missing.l,data.model=sim.data.model,imps=2)

results.l <- lapply(missing.l,runMI,data.model=sim.data.model,miPackage="amelia",m=2)
results.l

test <- run(sim.data.model, complete.l[[1]])
coef.l <- vectorize.object(test@coef, Labels)
      se.l <- vectorize.object(test@se, Labels)
      fit.l <- test@fit
      
      
mtrace(runMI)

mpi.close.Rslaves()
mpi.quit()
