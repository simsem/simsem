setwd("/nfs/home/patr1ckm/simsem/trunk/")

install.packages("simsem_0.0-8.tar.gz", repos=NULL, type="source")
library(simsem)
library(debug)
library(Rmpi)

source('AllClass.R')
source("AllGenerics.R")
source('vectorize.object-methods.R')
source( "make.labels-methods.R" )
source("is.null.object-methods.R")
source("simModel-methods.R")
source("run-methods.R")
source("combine.object-methods.R")
source("constrain.matrices-methods.R")
source("adjust.object-methods.R")
source("create.implied.MACS-methods.R"  )
source("runLavaan.R")
source("blank.parameters.R" )
source("write.lavaan.code.R" )
source("write.lavaan.constraint.R" )
source("collapse.exo.R")
source("extract.lavaan.summary.R" )
source("combine.object-methods.R")
source("find.indicator.var.R")
 source( "find.cutoff-methods.R")
 source("find.factor.mean.R")
 source("find.factor.var.R")
 source("find.fit.indices.OpenMx.R")
 source("find.indicator.mean.R")
 source("find.indicator.var.R")
 source("find.latent.error.var.R")
 source("find.latent.intercept.R")
 source("find.measurement.error.var.R")
 source("find.measurement.intercept.R")
 source("find.OpenMx.values-methods.R")        
source("find.possible.latent.cor.R")
source("validate.object.R")
source("validate.path.R")
source("validate.covariance.R")
source("vector.object.R")
source("vectorize.object-methods.R") 
source("reduce.matrices.R")
source("create.free.parameters.R")

# Generate complete data -
example.cfa <- function(x) {
  loading <- matrix(0,9,3)
  loading[1:3, 1] <- NA
  loading[4:6, 2] <- NA
  loading[7:9, 3] <- NA

  LX <- simMatrix(loading, x)

  error.cor <- diag(9)
  RTD <- symMatrix(error.cor)

  latent.cor <- matrix(NA, 3, 3)
  diag(latent.cor) <- 1

  RPH <- symMatrix(latent.cor, 0.5)

  CFA.model <- simSetCFA(LX = LX, RPH=RPH, RTD = RTD)
}

bigsim <- function(x) {
  CFA.model <- example.cfa(x)

  SimData <- simData(200, CFA.model)
  SimModel <- simModel(CFA.model)

  samp <- run(SimData)
  out.1 <- run(SimModel, run(SimData))

  out.ssd <- simResult(10, SimData, SimModel, multicore=TRUE)

  SimMissing <- simMissing(nforms=3, numImps=5)

  out.mis <- simResult(100, SimData, SimModel, SimMissing, multicore=TRUE)
}

floadings <- seq(.5,.9,by=.025)

mpi.bcast.cmd(cmd=library(simsem))
mpi.bcast.Robj2slave(example.cfa)

out.mis.l <- mpi.iapplyLB(floadings,bigsim)


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

TEcons <- matrix(0, 4, 2)
TEcons[1,] <- c(1, 1)
TEcons[2,] <- c(2, 2)
TEcons[3,] <- c(3, 3)
TEcons[4,] <- c(4, 4)
rownames(TEcons) <- rep("TE", 4)

equal.TE <- simEqualCon(TEcons, modelType="CFA")
sim.data.model1 <- simModel(data.model)
sim.data.model1@equalCon <- equal.TE
sim.data.model1@param@PS[1,2]<-0
sim.data.model1@param@PS[2,1]<-0

output1<-simResult(data.object,sim.data.model, nRep=30, seed=123456, multicore=TRUE)
output2<-simResult(data.object,sim.data.model1, nRep=30, seed=123456, multicore=TRUE)
anova(output1,output2)
runRep(data.object,data.model)

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
