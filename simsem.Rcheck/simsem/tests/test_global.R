#library(devtools)
#load_all("../../../simsem")

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

source("../../R/summary-methods.R")
source("../../R/summary-methods.R")
source("../../R/getKeywords.R")
source("../../R/getCutoff-methods.R")
source("../../R/clean.R")
source("../../R/summaryParam-methods.R")


context("Examples")

context("CFA")

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LY <- bind(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

RTE <- binds(diag(NA,6),1)

CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA")

Output <- sim(10, CFA.Model,n=300)


loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:5, 2] <- "a1"
loading[6,2] <- "a2"
lmis <- matrix("",6,2)
lmis[4:5,2] <- "runif(1,.01,.02)"
LY <- bind(loading, "runif(1,.6,.8)",lmis)

loading[1,1] <- "a3"
LY2 <- bind(loading, "runif(1,.6,.8)",lmis)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- bind(latent.cor, 0.5,symmetric=TRUE)

error.cor <- matrix(0, 6, 6)
diag(error.cor) <- NA
RTE <- bind(error.cor,popParam=1,"runif(1,0.01,0.015)",symmetric=TRUE)

tcfa <- model(LY=LY,RPS=RPS,RTE=RTE, modelType="CFA")
tcfamg <- model(LY=c(LY,LY2),RPS=RPS,RTE=RTE, modelType="CFA")
tcfamg2 <- model(LY=list(LY,LY),RPS=list(RPS,RPS),RTE=RTE, modelType="CFA")

out <- sim(10,tcfa,200)
out <- sim(10,tcfamg,200)
out <- sim(10,tcfamg2,200)

expect_is(cfa(),"SimResult")

loading <- matrix(0, 9, 3)
loading[1:3, 1] <- c(1, NA, NA)
loading[4:6, 2] <- c(1, NA, NA)
loading[7:9, 3] <- c(1, NA, NA)
loadingVal <- matrix(0, 9, 3)
loadingVal[2:3, 1] <- c(0.6, 0.7)
loadingVal[5:6, 2] <- c(1.1, 0.9)
loadingVal[8:9, 3] <- c(1.2, 1.1)
LY <- bind(loading, loadingVal)

facCov <- matrix(NA, 3, 3)
facCovVal <- diag(c(0.8, 0.9, 0.4))
facCovVal[lower.tri(facCovVal)] <- c(0.4, 0.2, 0.3)
facCovVal[upper.tri(facCovVal)] <- c(0.4, 0.2, 0.3)
PS <- bind(facCov, facCovVal)

errorCov <- diag(NA, 9)
errorCovVal <- diag(c(0.5, 1.1, 0.8, 0.4, 0.4, 0.8, 0.8, 0.5, 0.6))
TE <- binds(errorCov, errorCovVal)

AL <- bind(rep(NA, 3), 0)
TY <- bind(c(0, NA, NA, 0, NA, NA, 0, NA, NA), 0)

tcfa2 <- model(LY=LY,PS=PS,TE=TE,AL=AL,TY=TY, modelType="CFA") 

Output <- sim(20, tcfa, n=300)
## getCutoff(Output, 0.05)
## plotCutoff(Output, 0.05)
## summaryParam(Output)

expect_is(Output,"SimResult")

ex3 <- function() {
  factor.loading <- matrix(NA, 4, 2)
  factor.loading[,1] <- 1
  factor.loading[,2] <- 0:3
  LY <- bind(factor.loading)

  factor.mean <- rep(NA, 2)
  factor.mean.starting <- c(5, 2)
  AL <- bind(factor.mean, factor.mean.starting)

  factor.var <- rep(NA, 2)
  factor.var.starting <- c(1, 0.25)
  VPS <- bind(factor.var, factor.var.starting)

  factor.cor <- matrix(NA, 2, 2)
  diag(factor.cor) <- 1
  RPS <- binds(factor.cor, 0.5)

  VTE <- bind(rep(NA, 4), 1.2)

  RTE <- binds(diag(4))

  TY <- bind(rep(0, 4))

  LCA.Model <- simSetCFA(LY=LY, RPS=RPS, VPS=VPS, AL=AL, VTE=VTE, RTE=RTE, TY=TY)

  Data.True <- simData(LCA.Model, 300)
  SimModel <- simModel(LCA.Model)
# Output <- simResult(1, Data.True, SimModel)
#getCutoff(Output, 0.05)
#plotCutoff(Output, 0.05)

u1 <- simUnif(-0.1, 0.1)

  loading.trivial <- matrix(0, 4, 2)
  loading.trivial[2:3, 2] <- NA
  loading.mis <- bind(loading.trivial, "runif(1,-0.1,0.1)")

  LCA.Mis <- simMisspecCFA(LY = loading.mis)

  Data.Mis <- simData(LCA.Model, 300, LCA.Mis, sequential=TRUE)

  Output.Mis <- simResult(2, Data.Mis, SimModel)#, multicore=TRUE)
  getCutoff(Output.Mis, 0.05)
  plotCutoff(Output.Mis, 0.05)
  summaryParam(Output.Mis)
  return(Output.Mis)
}

expect_is(ex3(),"SimResult")


ex4 <- function() {
#u35 <- simUnif(0.3, 0.5)
#u57 <- simUnif(0.5, 0.7)
#u1 <- simUnif(-0.1, 0.1)
#n31 <- simNorm(0.3, 0.1)

path.BE <- matrix(0, 4, 4)
path.BE[3, 1:2] <- NA
path.BE[4, 3] <- NA
starting.BE <- matrix("", 4, 4)
starting.BE[3, 1:2] <- "runif(1,0.3,0.5)"
starting.BE[4, 3] <- "runif(1,0.5,0.7)"
BE <- bind(path.BE, starting.BE)

residual.error <- diag(4)
residual.error[1,2] <- residual.error[2,1] <- NA
RPS <- binds(residual.error, "rnorm(1,0.3,0.1)")

ME <- bind(rep(NA, 4), 0)

Path.Model <- simSetPath(RPS = RPS, BE = BE, ME = ME)

mis.path.BE <- matrix(0, 4, 4)
mis.path.BE[4, 1:2] <- NA
mis.BE <- bind(mis.path.BE, "runif(1,-.01,0.1)")
Path.Mis.Model <- simMisspecPath(BE = mis.BE, misfitType="rmsea", optMisfit="max", numIter=10) #, misfitBound=c(0.05, 0.08))

Data <- simData(Path.Model, 500, maxDraw=1000)
Data.Mis <- simData(Path.Model, 500, Path.Mis.Model)

# dat <- run(Data.Mis)
# x <- drawParametersMisspec(Path.Model, Path.Mis.Model)
# y <- simResultParam(1000, Path.Model, Path.Mis.Model)
# plot(y@misspec[,2], y@fit[,2])
# lines(loess.smooth(y@misspec[,2], y@fit[,2]), col="red")

SimModel <- simModel(Path.Model)
popMisfit(Path.Model, Path.Mis.Model, fit.measures="rmsea")

#Output <- simResult(100, Data, SimModel)
Output <- simResult(2, Data.Mis, SimModel)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summaryParam(Output)
return(Output)
}

expect_is(ex4(),"SimResult")

ex4.2 <- function() {
  #u35 <- simUnif(0.3, 0.5)
#u57 <- simUnif(0.5, 0.7)
#u1 <- simUnif(-0.1, 0.1)
#n31 <- simNorm(0.3, 0.1)

path.GA <- matrix(0, 2, 2)
path.GA[1, 1:2] <- NA
GA <- bind(path.GA, "runif(1,0.3,0.5)")

path.BE <- matrix(0, 2, 2)
path.BE[2, 1] <- NA
BE <- bind(path.BE, "runif(1,0.5,0.7)")

exo.cor <- matrix(NA, 2, 2)
diag(exo.cor) <- 1
RPH <- binds(exo.cor, "rnorm(1,0.3,0.1)")

RPS <- binds(diag(2))

Path.Model <- simSetPath(RPS = RPS, BE = BE, RPH = RPH, GA = GA, exo=TRUE)

mis.path.GA <- matrix(0, 2, 2)
mis.path.GA[2, 1:2] <- NA
mis.GA <- bind(mis.path.GA, "runif(1,-0.1,0.1)")
Path.Mis.Model <- simMisspecPath(GA = mis.GA, exo=TRUE)

Data.Mis <- simData(Path.Model, 500, Path.Mis.Model)
SimModel <- simModel(Path.Model)


Output <- simResult(1, Data.Mis, SimModel)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summaryParam(Output)
return(Output)
}

expect_is(ex4.2(),"SimResult")


ex5 <- function() {
#n65 <- simNorm(0.6, 0.05)
#u35 <- simUnif(0.3, 0.5)
#u68 <- simUnif(0.6, 0.8)
#u2 <- simUnif(-0.2, 0.2)
#n1 <- simNorm(0, 0.1)

loading <- matrix(0, 8, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:8, 3] <- NA
loading.start <- matrix("", 8, 3)
loading.start[1:3, 1] <- 0.7
loading.start[4:6, 2] <- 0.7
loading.start[7:8, 3] <- "rnorm(1,0.6,0.05)"
LY <- bind(loading, loading.start)

RTE <- binds(diag(8))

factor.cor <- diag(3)
factor.cor[1, 2] <- factor.cor[2, 1] <- NA
RPS <- binds(factor.cor, 0.5)

path <- matrix(0, 3, 3)
path[3, 1:2] <- NA
path.start <- matrix(0, 3, 3)
path.start[3, 1] <- "rnorm(1,0.6,0.05)"
path.start[3, 2] <- "runif(1,0.3,0.5)"
BE <- bind(path, path.start)

SEM.model <- simSetSEM(BE=BE, LY=LY, RPS=RPS, RTE=RTE)

loading.trivial <- matrix(NA, 8, 3)
loading.trivial[is.na(loading)] <- 0
LY.trivial <- bind(loading.trivial, "runif(1,-0.2,0.2)")

error.cor.trivial <- matrix(NA, 8, 8)
diag(error.cor.trivial) <- 0
RTE.trivial <- binds(error.cor.trivial, "rnorm(1,0,0.1)")

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

dat <- run(Data.Mis.Con)

Model.Original <- simModel(SEM.model)
Model.Con <- simModel(SEM.model, equalCon=equal.loading)


Output <- simResult(1, Data.Mis.Con, Model.Con) #, multicore=TRUE)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summaryParam(Output)
return(Output)
}

expect_is(ex5(),"SimResult")
context("Constraints on exogenous factors")

ex5.2 <- function() {
#n65 <- simNorm(0.6, 0.05)
#u35 <- simUnif(0.3, 0.5)
#u68 <- simUnif(0.6, 0.8)
#u2 <- simUnif(-0.2, 0.2)
#n1 <- simNorm(0, 0.1)

loading.X <- matrix(0, 6, 2)
loading.X[1:3, 1] <- NA
loading.X[4:6, 2] <- NA
LX <- bind(loading.X, 0.7)

loading.Y <- matrix(NA, 2, 1)
LY <- bind(loading.Y, "runif(1,0.6,0.8)")

RTD <- binds(diag(6))

RTE <- binds(diag(2))

factor.K.cor <- matrix(NA, 2, 2)
diag(factor.K.cor) <- 1
RPH <- binds(factor.K.cor, 0.5)

RPS <- binds(as.matrix(1))

path.GA <- matrix(NA, 1, 2)
path.GA.start <- matrix(c("rnorm(1,0.6,0.5)", "runif(1,0.3,0.5)"), ncol=2)
GA <- bind(path.GA, path.GA.start)

BE <- bind(as.matrix(0))

SEM.model <- simSetSEM(GA=GA, BE=BE, LX=LX, LY=LY, RPH=RPH, RPS=RPS, RTD=RTD, RTE=RTE, exo=TRUE)

loading.X.trivial <- matrix(NA, 6, 2)
loading.X.trivial[is.na(loading.X)] <- 0
LX.trivial <- bind(loading.X.trivial, "runif(1,-0.2,0.2)")

error.cor.X.trivial <- matrix(NA, 6, 6)
diag(error.cor.X.trivial) <- 0
RTD.trivial <- binds(error.cor.X.trivial, "rnorm(1,0,.1)")

error.cor.Y.trivial <- matrix(NA, 2, 2)
diag(error.cor.Y.trivial) <- 0
RTE.trivial <- binds(error.cor.Y.trivial, "rnorm(1,0,.1)")

RTH.trivial <- bind(matrix(NA, 6, 2), "rnorm(1,0,.1)")

SEM.Mis.Model <- simMisspecSEM(LX = LX.trivial, RTE = RTE.trivial, RTD = RTD.trivial, RTH = 
RTH.trivial, exo=TRUE)

constraint1 <- matrix(1, 3, 2)
constraint1[,1] <- 1:3
rownames(constraint1) <- rep("LY", 3)
constraint2 <- matrix(2, 3, 2)
constraint2[,1] <- 4:6
rownames(constraint2) <- rep("LY", 3)
constraint3 <- matrix(3, 2, 2)
constraint3[,1] <- 7:8
rownames(constraint3) <- rep("LY", 2)
equal.loading <- simEqualCon(constraint1, constraint2, constraint3, modelType="SEM.exo")
#equal.loading <- simEqualCon(constraint, modelType="SEM.exo")

Data.Original <- simData(SEM.model, 300)
Data.Mis <- simData(SEM.model, 300, misspec=SEM.Mis.Model)
Data.Con <- simData(SEM.model, 300, equalCon=equal.loading)
Data.Mis.Con <- simData(SEM.model, 300, misspec=SEM.Mis.Model, 
	equalCon=equal.loading)

Model.Original <- simModel(SEM.model)
Model.Con <- simModel(SEM.model, equalCon=equal.loading)

Output <- simResult(2, Data.Mis.Con, Model.Con)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summaryParam(Output)
return(Output)
}

expect_is(ex5.2(),"SimResult")

ex6 <- function() {
u2 <- simUnif(-0.2, 0.2)
n1 <- simNorm(0, 0.1)
u79 <- simUnif(0.7, 0.9)

loading.null <- matrix(0, 6, 1)
loading.null[1:6, 1] <- NA
LX.NULL <- bind(loading.null, 0.7)
RPH.NULL <- binds(diag(1))
RTD <- binds(diag(6))
CFA.Model.NULL <- simSetCFA(LY = LX.NULL, RPS = RPH.NULL, RTE = RTD)

error.cor.mis <- matrix(NA, 6, 6)
diag(error.cor.mis) <- 1
RTD.Mis <- binds(error.cor.mis, "rnorm(1,0,0.1)")
CFA.Model.NULL.Mis <- simMisspecCFA(RTE = RTD.Mis)

SimData.NULL <- simData(CFA.Model.NULL, 500, misspec = CFA.Model.NULL.Mis)
SimModel <- simModel(CFA.Model.NULL)
Output.NULL <- simResult(2, SimData.NULL, SimModel)

loading.alt <- matrix(0, 6, 2)
loading.alt[1:3, 1] <- NA
loading.alt[4:6, 2] <- NA
LX.ALT <- bind(loading.alt, 0.7)
latent.cor.alt <- matrix(NA, 2, 2)
diag(latent.cor.alt) <- 1
RPH.ALT <- binds(latent.cor.alt, "runif(1,0.7,0.9)")
CFA.Model.ALT <- simSetCFA(LY = LX.ALT, RPS = RPH.ALT, RTE = RTD)

loading.alt.mis <- matrix(NA, 6, 2)
loading.alt.mis[is.na(loading.alt)] <- 0
LX.alt.mis <- bind(loading.alt.mis, "runif(1,-.2,.2)")
CFA.Model.alt.mis <- simMisspecCFA(LY = LX.alt.mis, RTE=RTD.Mis)

SimData.ALT <- simData(CFA.Model.ALT, 500, misspec = CFA.Model.alt.mis)
Output.ALT <- simResult(2, SimData.ALT, SimModel)

cutoff <- getCutoff(Output.NULL, 0.05)
getPower(Output.ALT, cutoff)
plotPower(Output.ALT, Output.NULL, 0.05)
plotPower(Output.ALT, Output.NULL, 0.05, usedFit=c("RMSEA", "SRMR", "CFI"))

cutoff2 <- c(RMSEA = 0.05, CFI = 0.95, TLI = 0.95, SRMR = 0.06)
getPower(Output.ALT, cutoff2)
plotPower(Output.ALT, cutoff2)
plotPower(Output.ALT, cutoff2, usedFit=c("RMSEA", "SRMR", "CFI"))
return(Output.ALT)
}

expect_is(ex6(),"SimResult")

ex7 <- function() {

loading <- matrix(0, 9, 4)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
loading[c(1, 4, 7), 4] <- NA
loading.v <- matrix(0, 9, 4)
loading.v[1:3, 1] <- "runif(1,.4,.9)"
loading.v[4:6, 2] <- "runif(1,.4,.9)"
loading.v[7:9, 3] <- "runif(1,.4,.9)"
loading.v[c(1, 4, 7), 4] <- "runif(1,.3,.6)"
LY <- bind(loading, loading.v)

faccor <- diag(4)
faccor[1, 2] <- faccor[2, 1] <- NA
faccor[1, 3] <- faccor[3, 1] <- NA
faccor[2, 3] <- faccor[3, 2] <- NA
faccor.v <- diag(4)
faccor.v[1, 2] <- faccor.v[2, 1] <- "rnorm(1,.4,.1)"
faccor.v[1, 3] <- faccor.v[3, 1] <- "rnorm(1,.2,.1)"
faccor.v[2, 3] <- faccor.v[3, 2] <- "rnorm(1,.3,.1)"
RPS <- binds(faccor, faccor.v)

RTE <- binds(diag(9))

mtmm.model <- simSetCFA(LY=LY, RPS=RPS, RTE=RTE)

error.cor.mis <- matrix(NA, 9, 9)
diag(error.cor.mis) <- 1
RTE.mis <- binds(error.cor.mis, "rnorm(1,0,.1)")
loading.mis <- matrix(NA, 9, 4)
loading.mis[is.na(loading)] <- 0
loading.mis[,4] <- 0
LY.mis <- bind(loading.mis, "runif(1,-.2,.2)")
mtmm.model.mis <- simMisspecCFA(RTE = RTE.mis, LY=LY.mis)

SimMissing <- simMissing(pmMCAR=0.2, numImps=0)

SimData <- simData(mtmm.model, 500, misspec = mtmm.model.mis)
SimModel <- simModel(mtmm.model)

Output <- simResult(1, SimData, SimModel, SimMissing)
#getCutoff(Output, 0.05)
#plotCutoff(Output, 0.05)
#summary(Output)
return(Output)
}

expect_is(ex7(),"SimResult")

ex8 <- function() {
  
loading <- matrix(0, 48, 4)
loading[1:12, 1] <- NA
loading[13:24, 2] <- NA
loading[25:36, 3] <- NA
loading[37:48, 4] <- NA
LY <- bind(loading, "runif(1,0.5,0.9)")

faccor <- matrix(NA, 4, 4)
diag(faccor) <- 1
RPS <- binds(faccor, "runif(1,.1,.6)")

RTE <- binds(diag(48))

CFA.model <- simSetCFA(LY=LY, RPS=RPS, RTE=RTE)

loading.mis <- matrix(NA, 48, 4)
loading.mis[is.na(loading)] <- 0
LY.mis <- bind(loading.mis, "runif(1,-.2,.2)")
CFA.model.mis <- simMisspecCFA(LY=LY.mis)

setx <- c(1:3, 13:15, 25:27, 37:39)
set1 <- setx + 3
set2 <- set1 + 3
set3 <- set2 + 3
itemGroups <- list(setx, set1, set2, set3)

SimMissing <- simMissing(nforms=3, itemGroups=itemGroups, numImps=0)

SimData <- simData(CFA.model, 1000) 
SimModel <- simModel(CFA.model)

#dat <- run(SimData)
#dat <- run(SimMissing, dat)
#out <- run(SimModel, dat, SimMissing)

Output <- simResult(1, SimData, SimModel)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)
return(Output)
}

expect_is(ex8(),"SimResult")

ex9 <- function() {
u2 <- simUnif(-0.2, 0.2)
u5 <- simUnif(-0.5, 0.5)
t2 <- simT(2)
t3 <- simT(3)
t4 <- simT(4)
t5 <- simT(5)
chi3 <- simChisq(3)
chi4 <- simChisq(4)
chi5 <- simChisq(5)
chi6 <- simChisq(6)

loading <- matrix(0, 12, 3)
loading[1:4, 1] <- NA
loading[5:8, 2] <- NA
loading[9:12, 3] <- NA
LX <- bind(loading, 0.7)

latent.cor <- matrix(NA, 3, 3)
diag(latent.cor) <- 1
RPH <- binds(latent.cor, "runif(1,-.5,.5)")

error.cor <- matrix(0, 12, 12)
diag(error.cor) <- 1
RTD <- binds(error.cor)

CFA.Model <- simSetCFA(LX = LX, RPH = RPH, RTD = RTD) 

loading.mis <- matrix(NA, 12, 3)
loading.mis[is.na(loading)] <- 0
LY.mis <- bind(loading.mis, "runif(1,-.2,.2)")
CFA.model.mis <- simMisspecCFA(LY=LY.mis)

SimDataDist <- simDataDist(t2, t3, t4, t5, chi3, chi4, chi5, chi6, chi3, chi4, chi5, chi6, reverse=c(rep(FALSE, 8), rep(TRUE, 4)))
SimData <- simData(CFA.Model, 200, misspec=CFA.model.mis, indDist=SimDataDist)
SimModel <- simModel(CFA.Model, estimator="mlm")

dat <- run(SimData)
SimData2 <- simData(CFA.Model, 200, misspec=CFA.model.mis, indDist=SimDataDist, modelBoot=TRUE, realData=dat)


Output <- simResult(1000, SimData2, SimModel)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)

g21 <- simGamma(2, 1)
n01 <- simNorm(0, 1)
object <- simDataDist(g21)
plotDist(object)
object2 <- simDataDist(g21, n01)
plotDist(object2, r=0.5)

g21 <- simGamma(2, 1)
n01 <- simNorm(0, 1)
chi2 <- simChisq(2)
obj <- simDataDist(g21, n01, chi2)
plotDist(obj, var=c(2,3))
return(Output)
}

# Needs copula / gsl to be installed on the cluster.
#expect_is(ex9(),"SimResult")

expect_is(ex10(),"SimResult")

expect_is(ex11(),"SimResult")
