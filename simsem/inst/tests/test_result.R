# Assess global performance

library(devtools)
library(simsem)

# Internals
source("../../R/AllClass.R")
source("../../R/AllGenerics.R")
source("../../R/model.R")
source("../../R/drawParam.R")
source("../../R/bind.R")
source("../../R/find.R")
source("../../R/validate.R")
source("../../R/createData.R")
source("../../R/simDist-constructor.R")
source("../../R/generate.R")
source("../../R/analyze.R")
source("../../R/miss.R")
source("../../R/sim.R")

# Summarization
source("../../R/summary-methods.R")
source("../../R/getKeywords.R")
source("../../R/getCutoff-methods.R")
source("../../R/clean.R")
source("../../R/summaryParam-methods.R")

# Sample Simulation - CFA

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LX <- simMatrix(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, 0.5)

error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
RTD <- symMatrix(error.cor)

CFA.Model <- simSetCFA(LY = LX, RPH = RPH, RTD = RTD)
SimData <- simData(CFA.Model, 200)
SimModel <- simModel(CFA.Model)

LY <- bind(loading,0.7)
RPS <- binds(latent.cor, 0.5)
RTE <- binds(error.cor,0.5)
tcfa <- model(LY=LY,RPS=RPS,RTE=RTE,modelType="CFA")

## SimMissing <- simMissing(pmMCAR=0.1, numImps=5)
context("Simple Simulation")
a <- system.time(
                 Output <- simResult(100, SimData, SimModel))
b <- system.time(
                 out <- sim(10,tcfa,200))

#cat(paste(a[[1]],"\n",sep=""))



## Output <- simResult(100, SimData, SimModel, SimMissing, multicore=TRUE)
## getCutoff(Output, 0.05)
## plotCutoff(Output, 0.05)
## summaryParam(Output)

# Trial 2: A little more advanced.
loading <- matrix(0, 8, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:8, 3] <- NA
loading.start <- matrix("", 8, 3)
loading.start[1:3, 1] <- 0.7
loading.start[4:6, 2] <- 0.7
loading.start[7:8, 3] <- "rnorm(1,0.6,0.05)"
LY <- simMatrix(loading, loading.start)

RTE <- symMatrix(diag(8))

factor.cor <- diag(3)
factor.cor[1, 2] <- factor.cor[2, 1] <- NA
RPS <- symMatrix(factor.cor, 0.5)

path <- matrix(0, 3, 3)
path[3, 1:2] <- NA
path.start <- matrix(0, 3, 3)
path.start[3, 1] <- "rnorm(1,0.6,0.05)"
path.start[3, 2] <- "runif(1,0.3,0.5)"
BE <- simMatrix(path, path.start)

SEM.model <- simSetSEM(BE=BE, LY=LY, RPS=RPS, RTE=RTE)

loading.trivial <- matrix(NA, 8, 3)
loading.trivial[is.na(loading)] <- 0
LY.trivial <- simMatrix(loading.trivial, "runif(1,-0.2,0.2)")

error.cor.trivial <- matrix(NA, 8, 8)
diag(error.cor.trivial) <- 0
RTE.trivial <- symMatrix(error.cor.trivial, "rnorm(1,0,0.1)")

SEM.Mis.Model <- simMisspecSEM(LY = LY.trivial, RTE = RTE.trivial, conBeforeMis=FALSE, misBeforeFill=TRUE)

constraint <- matrix(0, 2, 2)
constraint[1,] <- c(7, 3)
constraint[2,] <- c(8, 3)
rownames(constraint) <- rep("LY", 2)
equal.loading <- simEqualCon(constraint, modelType="SEM", conBeforeFill=FALSE)

Data.Original <- simData(SEM.model, 300)
Data.Mis <- simData(SEM.model, 300, misspec=SEM.Mis.Model)
Data.Con <- simData(SEM.model, 300, equalCon=equal.loading)
Data.Mis.Con <- simData(SEM.model, 300, misspec=SEM.Mis.Model, equalCon=equal.loading)


Model.Original <- simModel(SEM.model)
Model.Con <- simModel(SEM.model, equalCon=equal.loading)

system.time(Output <- simResult(100, Data.Mis.Con, Model.Con)) #, multicore=TRUE)
# 56.49

loading[7:8,3] <- "con1"
LY <- bind(loading, loading.start, "runif(1,-0.2,0.2)")
rte <- diag(8)
diag(rte) <- NA
RTE <- binds(rte,1,"rnorm(1,0,0.1)")
RPS <- binds(factor.cor, 0.5)
BE <- bind(path, path.start)

tsem <- model(LY=LY, RTE=RTE, RPS=RPS, BE=BE, modelType="SEM")
system.time(out <- sim(100,tsem,300))
# 45.439
