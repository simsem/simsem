pkgname <- "simsem"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('simsem')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("MatrixSet-class")
### * MatrixSet-class

flush(stderr()); flush(stdout())

### Name: MatrixSet-class
### Title: Class '"MatrixSet"'
### Aliases: MatrixSet-class MisspecSet-class summary,MatrixSet-method
###   summary,MisspecSet-method

### ** Examples

showClass("SimSet")

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- simMatrix(loading, loadingValues)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, 0.5)
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
RTD <- symMatrix(error.cor)
CFA.Model <- simSetCFA(LX = LX, RPH = RPH, RTD = RTD)
MatrixSet <- run(CFA.Model)
summary(MatrixSet)



cleanEx()
nameEx("Null-class")
### * Null-class

flush(stderr()); flush(stdout())

### Name: Nullclass
### Title: Null Objects
### Aliases: NullDataFrame-class NullVector-class NullMatrix-class
###   NullSimMatrix-class NullSymMatrix-class NullSimVector-class
###   NullSimSet-class NullSimEqualCon-class NullSimREqualCon-class
###   NullRSet-class NullSimMisspec-class NullSimDataDist-class
###   NullSimMissing-class NullSimFunction-class
### Keywords: classes

### ** Examples

# No example



cleanEx()
nameEx("SimData-class")
### * SimData-class

flush(stderr()); flush(stdout())

### Name: SimData-class
### Title: Class '"SimData"'
### Aliases: SimData-class run,SimData-method summary,SimData-method
### Keywords: classes

### ** Examples

showClass("SimData")
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- simMatrix(loading, loadingValues)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, 0.5)
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
RTD <- symMatrix(error.cor)
CFA.Model <- simSetCFA(LY = LX, RPS = RPH, RTE = RTD)
SimData <- simData(CFA.Model, 200)
summary(SimData)
run(SimData)



cleanEx()
nameEx("SimDataDist-class")
### * SimDataDist-class

flush(stderr()); flush(stdout())

### Name: SimDataDist-class
### Title: Class '"SimDataDist"'
### Aliases: SimDataDist-class summary,SimDataDist-method
###   run,SimDataDist-method plotDist,SimDataDist-method
###   extract,SimDataDist-method
### Keywords: classes

### ** Examples

showClass("SimDataDist")

chisq3 <- simChisq(3)
chisq8 <- simChisq(8)
dist <- simDataDist(chisq3, chisq8)
dist2 <- extract(dist, 2)

m <- c(0, 0)
cm <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
n <- 20
dat <- run(dist, n, m, cm)

plotDist(dist, r=0.2)




cleanEx()
nameEx("SimDataOut-class")
### * SimDataOut-class

flush(stderr()); flush(stdout())

### Name: SimDataOut-class
### Title: Class '"SimDataOut"'
### Aliases: SimDataOut-class summary,SimDataOut-method
###   createImpliedMACS,SimDataOut-method
###   summaryPopulation,SimDataOut-method getPopulation,SimDataOut-method
### Keywords: classes

### ** Examples

showClass("SimDataOut")
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- simMatrix(loading, 0.7)
RPH <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model <- simSetCFA(LY = LX, RPS = RPH, RTE = RTD)
SimData <- simData(CFA.Model, 500)
SimModel <- simModel(CFA.Model)
Data <- run(SimData, dataOnly=FALSE)
Result <- run(SimModel, Data)
summary(Data)
summaryPopulation(Data)
mis <- getPopulation(Data, misspec=TRUE)



cleanEx()
nameEx("SimEqualCon-class")
### * SimEqualCon-class

flush(stderr()); flush(stdout())

### Name: SimEqualCon-class
### Title: Class '"SimEqualCon"'
### Aliases: SimEqualCon-class summary,SimEqualCon-method
### Keywords: classes

### ** Examples

showClass("SimEqualCon")
constraint1 <- matrix(1, 3, 2)
constraint1[,1] <- 1:3
rownames(constraint1) <- rep("LY", 3)
constraint2 <- matrix(2, 3, 2)
constraint2[,1] <- 4:6
rownames(constraint2) <- rep("LY", 3)
constraint3 <- matrix(3, 2, 2)
constraint3[,1] <- 7:8
rownames(constraint3) <- rep("LY", 2)
equal.loading <- simEqualCon(constraint1, constraint2, constraint3, modelType="SEM")
summary(equal.loading)



cleanEx()
nameEx("SimFunction-class")
### * SimFunction-class

flush(stderr()); flush(stdout())

### Name: SimFunction-class
### Title: Class '"SimFunction"'
### Aliases: SimFunction-class summary,SimFunction-method
###   run,SimFunction-method
### Keywords: classes

### ** Examples

showClass("SimFunction")

n65 <- simNorm(0.6, 0.05)
u35 <- simUnif(0.3, 0.5)
u68 <- simUnif(0.6, 0.8)
u2 <- simUnif(-0.2, 0.2)
n1 <- simNorm(0, 0.1)

loading <- matrix(0, 9, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
loading.start <- matrix("", 9, 3)
loading.start[1:3, 1] <- 0.7
loading.start[4:6, 2] <- 0.7
loading.start[7:9, 3] <- "u68"
LY <- simMatrix(loading, loading.start)

RTE <- symMatrix(diag(9))

factor.cor <- diag(3)
factor.cor[1, 2] <- factor.cor[2, 1] <- NA
RPS <- symMatrix(factor.cor, 0.5)

path <- matrix(0, 3, 3)
path[3, 1:2] <- NA
path.start <- matrix(0, 3, 3)
path.start[3, 1] <- "n65"
path.start[3, 2] <- "u35"
BE <- simMatrix(path, path.start)

datGen <- simSetSEM(BE=BE, LY=LY, RPS=RPS, RTE=RTE)

loading.trivial <- matrix(NA, 9, 3)
loading.trivial[is.na(loading)] <- 0
LY.trivial <- simMatrix(loading.trivial, "u2")

error.cor.trivial <- matrix(NA, 9, 9)
diag(error.cor.trivial) <- 0
RTE.trivial <- symMatrix(error.cor.trivial, "n1")

misGen <- simMisspecSEM(LY = LY.trivial, RTE = RTE.trivial)

Data.Mis <- simData(datGen, 300, misspec=misGen)

loading <- matrix(0, 12, 4)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 4] <- NA
loading[10:12, 3] <- NA

path <- matrix(0, 4, 4)
path[4, 1:3] <- NA

analysis <- simParamSEM(BE=path, LY=loading)

Model <- simModel(analysis)

fun <- simFunction(indProd, var1=paste("y", 1:3, sep=""), var2=paste("y", 4:6, sep=""), namesProd=paste("y", 10:12, sep=""))

# Real simulation will need more than just 10 replications
Output <- simResult(10, Data.Mis, Model, objFunction=fun)
summary(Output)

# Example of using the simfunction
mc <- simFunction(indProd, var1=1:3, var2=4:6)
run(mc, attitude[,-1])
summary(mc)



cleanEx()
nameEx("SimGenLabels-class")
### * SimGenLabels-class

flush(stderr()); flush(stdout())

### Name: SimGenLabels-class
### Title: Class '"SimGenLabels"'
### Aliases: SimGenLabels-class run,SimGenLabels-method
### Keywords: classes

### ** Examples

# No example



cleanEx()
nameEx("SimMatrix-class")
### * SimMatrix-class

flush(stderr()); flush(stdout())

### Name: SimMatrix-class
### Title: Matrix object: Random parameters matrix
### Aliases: SimMatrix-class run,SimMatrix-method
###   summaryShort,SimMatrix-method summary,SimMatrix-method
###   extract,SimMatrix-method
### Keywords: classes

### ** Examples

showClass("SimMatrix")

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- simMatrix(loading, loadingValues)
summary(LX)
run(LX)

n65 <- simNorm(0.6, 0.05)
LY <- simMatrix(loading, "n65")
summary(LY)
run(LY)

u34 <- simUnif(0.3, 0.4)
LY <- adjust(LY, "u34", c(2, 1))
summary(LY)
run(LY)
summaryShort(LY)

LY <- extract(LY, 1:3, 1)
summary(LY)



cleanEx()
nameEx("SimMissing-class")
### * SimMissing-class

flush(stderr()); flush(stdout())

### Name: SimMissing-class
### Title: Class '"SimMissing"'
### Aliases: SimMissing-class summary,SimMissing-method
###   run,SimMissing-method
### Keywords: classes

### ** Examples

# No Example



cleanEx()
nameEx("SimMisspec-class")
### * SimMisspec-class

flush(stderr()); flush(stdout())

### Name: SimMisspec-class
### Title: Class '"SimMisspec"'
### Aliases: SimMisspec-class run,SimMisspec-method
###   summary,SimMisspec-method
### Keywords: classes

### ** Examples

showClass("SimMisspec")
n01 <- simNorm(0, 0.1)
error.cor.Mis <- matrix(NA, 6, 6)
diag(error.cor.Mis) <- 1
RTD.Mis <- symMatrix(error.cor.Mis, "n01")
CFA.Model.Mis <- simMisspecCFA(RTD=RTD.Mis)



cleanEx()
nameEx("SimModel-class")
### * SimModel-class

flush(stderr()); flush(stdout())

### Name: SimModel-class
### Title: Class '"SimModel"'
### Aliases: SimModel-class run,SimModel-method summary,SimModel-method
### Keywords: classes

### ** Examples

showClass("SimModel")
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- simMatrix(loading, loadingValues)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, 0.5)
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
RTD <- symMatrix(error.cor)
CFA.Model <- simSetCFA(LX = LX, RPH = RPH, RTD = RTD)
SimModel <- simModel(CFA.Model)
summary(SimModel)



cleanEx()
nameEx("SimModelMIOut-class")
### * SimModelMIOut-class

flush(stderr()); flush(stdout())

### Name: SimModelMIOut-class
### Title: Class '"SimModelMIOut"'
### Aliases: SimModelMIOut-class
### Keywords: classes

### ** Examples

showClass("SimModelMIOut")
showClass("SimResult")
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- simMatrix(loading, 0.7)
RPH <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model <- simSetCFA(LY = LX, RPS = RPH, RTE = RTD)
SimData <- simData(CFA.Model, 500)
SimModel <- simModel(CFA.Model)
SimMissing <- simMissing(pmMCAR=0.05, numImps=5)
Data <- run(SimData)
Data <- run(SimMissing, Data)
Result <- run(SimModel, Data, SimMissing)
summary(Result)



cleanEx()
nameEx("SimModelOut-class")
### * SimModelOut-class

flush(stderr()); flush(stdout())

### Name: SimModelOut-class
### Title: Class '"SimModelOut"'
### Aliases: SimModelOut-class summary,SimModelOut-method
###   createImpliedMACS,SimModelOut-method
###   summaryPopulation,SimModelOut-method getPopulation,SimModelOut-method
###   setPopulation,SimModelOut,SimRSet-method
###   setPopulation,SimModelOut,SimSet-method
### Keywords: classes

### ** Examples

showClass("SimModelOut")
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- simMatrix(loading, 0.7)
RPH <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model <- simSetCFA(LY = LX, RPS = RPH, RTE = RTD)
SimData <- simData(CFA.Model, 500)
SimModel <- simModel(CFA.Model)
Data <- run(SimData)
Result <- run(SimModel, Data)
summary(Result)
summaryParam(Result)
summaryPopulation(Result)
param <- getPopulation(Result)
Result2 <- setPopulation(Result, param)
Result3 <- setPopulation(Result, CFA.Model)



cleanEx()
nameEx("SimParam-class")
### * SimParam-class

flush(stderr()); flush(stdout())

### Name: SimParam-class
### Title: Class '"SimParam"'
### Aliases: SimParam-class summary,SimParam-method

### ** Examples

showClass("SimParam")

library(lavaan)
loading <- matrix(0, 9, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
HS.Model <- simParamCFA(LX = loading)
summary(HS.Model)
SimModel <- simModel(HS.Model, indLab=paste("x", 1:9, sep=""))
out <- run(SimModel, HolzingerSwineford1939)
summary(out)

HS.Model2 <- extract(HS.Model, y=1:3)
summary(HS.Model2)



cleanEx()
nameEx("SimREqualCon-class")
### * SimREqualCon-class

flush(stderr()); flush(stdout())

### Name: SimREqualCon-class
### Title: Class '"SimREqualCon"'
### Aliases: SimREqualCon-class summary,SimREqualCon-method
### Keywords: classes

### ** Examples

# No example



cleanEx()
nameEx("SimResult-class")
### * SimResult-class

flush(stderr()); flush(stdout())

### Name: SimResult-class
### Title: Class '"SimResult"'
### Aliases: SimResult-class summary,SimResult-method
###   summaryPopulation,SimResult-method getPopulation,SimResult-method
###   setPopulation,SimResult,data.frame-method
###   setPopulation,SimResult,SimSet-method
###   setPopulation,SimResult,VirtualRSet-method
### Keywords: classes

### ** Examples

showClass("SimResult")
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- simMatrix(loading, 0.7)
RPH <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model <- simSetCFA(LY = LX, RPS = RPH, RTE = RTD)
SimData <- simData(CFA.Model, 500)
SimModel <- simModel(CFA.Model)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
Output <- simResult(50, SimData, SimModel)
summary(Output)
getCutoff(Output, 0.05)
summaryParam(Output)
summaryPopulation(Output)
param <- getPopulation(Output)
Output <- setPopulation(Output, param)
Output2 <- setPopulation(Output, CFA.Model)



cleanEx()
nameEx("SimResultParam-class")
### * SimResultParam-class

flush(stderr()); flush(stdout())

### Name: SimResultParam-class
### Title: Class '"SimResultParam"'
### Aliases: SimResultParam-class summary,SimResultParam-method
###   summaryParam,SimResultParam-method
### Keywords: classes

### ** Examples

showClass("SimResultParam")

u35 <- simUnif(0.3, 0.5)
u57 <- simUnif(0.5, 0.7)
u1 <- simUnif(-0.1, 0.1)
n31 <- simNorm(0.3, 0.1)

path.BE <- matrix(0, 4, 4)
path.BE[3, 1:2] <- NA
path.BE[4, 3] <- NA
starting.BE <- matrix("", 4, 4)
starting.BE[3, 1:2] <- "u35"
starting.BE[4, 3] <- "u57"
BE <- simMatrix(path.BE, starting.BE)

residual.error <- diag(4)
residual.error[1,2] <- residual.error[2,1] <- NA
RPS <- symMatrix(residual.error, "n31")

ME <- simVector(rep(NA, 4), 0)

Path.Model <- simSetPath(RPS = RPS, BE = BE, ME = ME)

mis.path.BE <- matrix(0, 4, 4)
mis.path.BE[4, 1:2] <- NA
mis.BE <- simMatrix(mis.path.BE, "u1")
Path.Mis.Model <- simMisspecPath(BE = mis.BE, misfitType="rmsea") #, misfitBound=c(0.05, 0.08))

# The number of replications in actual analysis should be much more than 50
ParamObject <- simResultParam(50, Path.Model, Path.Mis.Model)
summary(ParamObject)



cleanEx()
nameEx("SimSet-class")
### * SimSet-class

flush(stderr()); flush(stdout())

### Name: SimSet-class
### Title: Class '"SimSet"'
### Aliases: SimSet-class run,SimSet-method summary,SimSet-method
###   extract,SimSet-method

### ** Examples

showClass("SimSet")

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- simMatrix(loading, loadingValues)
summary(LX)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, 0.5)

# Error Correlation Object
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
RTD <- symMatrix(error.cor)

CFA.Model <- simSetCFA(LX = LX, RPH = RPH, RTD = RTD)
summary(CFA.Model)
#run(CFA.Model)

CFA.Model2 <- extract(CFA.Model, y=1:3, e=1)
summary(CFA.Model2)



cleanEx()
nameEx("SimVector-class")
### * SimVector-class

flush(stderr()); flush(stdout())

### Name: SimVector-class
### Title: Vector object: Random parameters vector
### Aliases: SimVector-class run,SimVector-method
###   summaryShort,SimVector-method summary,SimVector-method
###   extract,SimVector-method
### Keywords: classes

### ** Examples

showClass("SimVector")

factor.mean <- rep(NA, 2)
factor.mean.starting <- c(5, 2)
AL <- simVector(factor.mean, factor.mean.starting)
run(AL)
summary(AL)
summaryShort(AL)

n01 <- simNorm(0, 1)
AL <- adjust(AL, "n01", 2)
run(AL)
summary(AL)

AL <- extract(AL, 1)
summary(AL)



cleanEx()
nameEx("SymMatrix-class")
### * SymMatrix-class

flush(stderr()); flush(stdout())

### Name: SymMatrix-class
### Title: Symmetric matrix object: Random parameters symmetric matrix
### Aliases: SymMatrix-class run,SymMatrix-method summary,SymMatrix-method
### Keywords: classes

### ** Examples

showClass("SymMatrix")

latent.cor <- matrix(NA, 3, 3)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, 0.5)

u46 <- simUnif(0.4, 0.6)
RPH <- adjust(RPH, "u46", c(3,2))
summary(RPH)
summaryShort(RPH)
run(RPH)



cleanEx()
nameEx("VirtualDist-class")
### * VirtualDist-class

flush(stderr()); flush(stdout())

### Name: VirtualDist-class
### Title: Distribution Objects
### Aliases: VirtualDist-class SimBeta-class SimBinom-class SimCauchy-class
###   SimChisq-class SimExp-class SimF-class SimGamma-class SimGeom-class
###   SimHyper-class SimLnorm-class SimLogis-class SimNbinom-class
###   SimNorm-class SimPois-class SimT-class SimUnif-class SimWeibull-class
###   run,SimBeta-method run,SimBinom-method run,SimCauchy-method
###   run,SimChisq-method run,SimExp-method run,SimF-method
###   run,SimGamma-method run,SimGeom-method run,SimHyper-method
###   run,SimLnorm-method run,SimLogis-method run,SimNbinom-method
###   run,SimNorm-method run,SimPois-method run,SimT-method
###   run,SimUnif-method run,SimWeibull-method summary,SimBeta-method
###   summary,SimBinom-method summary,SimCauchy-method
###   summary,SimChisq-method summary,SimExp-method summary,SimF-method
###   summary,SimGamma-method summary,SimGeom-method
###   summary,SimHyper-method summary,SimLnorm-method
###   summary,SimLogis-method summary,SimNbinom-method
###   summary,SimNorm-method summary,SimPois-method summary,SimT-method
###   summary,SimUnif-method summary,SimWeibull-method
###   summaryShort,SimBeta-method summaryShort,SimBinom-method
###   summaryShort,SimCauchy-method summaryShort,SimChisq-method
###   summaryShort,SimExp-method summaryShort,SimF-method
###   summaryShort,SimGamma-method summaryShort,SimGeom-method
###   summaryShort,SimHyper-method summaryShort,SimLnorm-method
###   summaryShort,SimLogis-method summaryShort,SimNbinom-method
###   summaryShort,SimNorm-method summaryShort,SimPois-method
###   summaryShort,SimT-method summaryShort,SimUnif-method
###   summaryShort,SimWeibull-method toFunction,SimBeta-method
###   toFunction,SimBinom-method toFunction,SimCauchy-method
###   toFunction,SimChisq-method toFunction,SimExp-method
###   toFunction,SimF-method toFunction,SimGamma-method
###   toFunction,SimGeom-method toFunction,SimHyper-method
###   toFunction,SimLnorm-method toFunction,SimLogis-method
###   toFunction,SimNbinom-method toFunction,SimNorm-method
###   toFunction,SimPois-method toFunction,SimT-method
###   toFunction,SimUnif-method toFunction,SimWeibull-method
###   plotDist,VirtualDist-method skew,VirtualDist-method
###   kurtosis,VirtualDist-method
### Keywords: classes

### ** Examples

showClass("VirtualDist")
u1 <- simUnif(0, 1)
chi3 <- simChisq(3)
summary(chi3)
skew(chi3)
kurtosis(chi3)
plotDist(chi3)
plotDist(chi3, reverse=TRUE)



cleanEx()
nameEx("VirtualRSet-class")
### * VirtualRSet-class

flush(stderr()); flush(stdout())

### Name: ParameterSet
### Title: Class '"VirtualRSet"', '"SimLabels"' and '"SimRSet"'
### Aliases: VirtualRSet-class SimRSet-class SimLabels-class
###   summary,VirtualRSet-method summary,SimRSet-method
###   summary,SimLabels-method

### ** Examples

# No example



cleanEx()
nameEx("adjust")
### * adjust

flush(stderr()); flush(stdout())

### Name: adjust
### Title: Change an element in 'SimMatrix', 'SymMatrix', or 'SimVector'.
### Aliases: adjust adjust-methods adjust,ANY-method
###   adjust,SimMatrix-method adjust,SymMatrix-method
###   adjust,SimVector-method

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LX <- simMatrix(loading, 0.7)
summary(LX)
run(LX)

u34 <- simUnif(0.3, 0.4)
LX <- adjust(LX, "u34", c(2, 1))
summary(LX)
run(LX)

LX <- adjust(LX, 0, c(2,1))
LX <- adjust(LX, 0.5, c(2,2), FALSE)
summary(LX)
run(LX)

factor.mean <- rep(NA, 2)
factor.mean.starting <- c(5, 2)
AL <- simVector(factor.mean, factor.mean.starting)
run(AL)
summary(AL)

n01 <- simNorm(0, 1)
AL <- adjust(AL, "n01", 2)
run(AL)
summary(AL)



cleanEx()
nameEx("anova")
### * anova

flush(stderr()); flush(stdout())

### Name: anova
### Title: Provide a comparison of nested models across replications
### Aliases: anova,SimResult-method anova,SimModelOut-method
###   anova,SimModelMIOut-method

### ** Examples

loading1 <- matrix(0, 6, 1)
loading1[1:6, 1] <- NA
loading2 <- loading1
loading2[6,1] <- 0
LX1 <- simMatrix(loading1, 0.7)
LX2 <- simMatrix(loading2, 0.7)
RPH <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model1 <- simSetCFA(LY = LX1, RPS = RPH, RTE = RTD)
CFA.Model2 <- simSetCFA(LY = LX2, RPS = RPH, RTE = RTD)
SimData <- simData(CFA.Model1, 500)
SimModel1 <- simModel(CFA.Model1)
SimModel2 <- simModel(CFA.Model2)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
# Need to make sure that both simResult calls have the same seed!
Output1 <- simResult(50, SimData, SimModel1, seed=123567)
Output2 <- simResult(50, SimData, SimModel2, seed=123567)
anova(Output1, Output2)



cleanEx()
nameEx("blankParameters")
### * blankParameters

flush(stderr()); flush(stdout())

### Name: blankParameters
### Title: Change all elements in the non-null objects to be all NAs.
### Aliases: blankParameters

### ** Examples

# No example



cleanEx()
nameEx("centralMoment")
### * centralMoment

flush(stderr()); flush(stdout())

### Name: centralMoment
### Title: Calculate central moments of a variable
### Aliases: centralMoment

### ** Examples

# This function is not public.

# centralMoment(1:5, 2)



cleanEx()
nameEx("checkInputValue")
### * checkInputValue

flush(stderr()); flush(stdout())

### Name: checkInputValue
### Title: Check the value argument in the matrix, symmetric matrix, or
###   vector objects
### Aliases: checkInputValue checkInputValueVector

### ** Examples

# No example



cleanEx()
nameEx("clean")
### * clean

flush(stderr()); flush(stdout())

### Name: clean
### Title: Extract only converged replications in the result object
### Aliases: clean

### ** Examples

# No example



cleanEx()
nameEx("collapseExo")
### * collapseExo

flush(stderr()); flush(stdout())

### Name: collapseExo
### Title: Collapse all exogenous variables and put all in endogenous side
###   only.
### Aliases: collapseExo

### ** Examples

# No example



cleanEx()
nameEx("combineLatentCorExoEndo")
### * combineLatentCorExoEndo

flush(stderr()); flush(stdout())

### Name: combineLatentCorExoEndo
### Title: Combine exogenous factor correlation and endogenous factor
###   correlation into a single matrix
### Aliases: combineLatentCorExoEndo

### ** Examples

# No example



cleanEx()
nameEx("combineLoadingExoEndo")
### * combineLoadingExoEndo

flush(stderr()); flush(stdout())

### Name: combineLoadingExoEndo
### Title: Combine factor loading from the exogenous and endogenous sides
###   into a single matrix
### Aliases: combineLoadingExoEndo

### ** Examples

# No example



cleanEx()
nameEx("combineMeasurementErrorExoEndo")
### * combineMeasurementErrorExoEndo

flush(stderr()); flush(stdout())

### Name: combineMeasurementErrorExoEndo
### Title: Combine measurement error correlation from the exogenous and
###   endogenous sides into a single matrix
### Aliases: combineMeasurementErrorExoEndo

### ** Examples

# No example



cleanEx()
nameEx("combineObject")
### * combineObject

flush(stderr()); flush(stdout())

### Name: combineObject
### Title: Combine by summing or binding two objects together.
### Aliases: combineObject combineObject-methods
###   combineObject,ANY,ANY-method combineObject,SimMatrix,SimMatrix-method
###   combineObject,SimVector,SimVector-method
###   combineObject,vector,vector-method combineObject,matrix,matrix-method
###   combineObject,MatrixSet,MatrixSet-method
###   combineObject,SimParam,list-method

### ** Examples

# No example



cleanEx()
nameEx("combinePathExoEndo")
### * combinePathExoEndo

flush(stderr()); flush(stdout())

### Name: combinePathExoEndo
### Title: Combine the regression coefficient matrices
### Aliases: combinePathExoEndo

### ** Examples

# No example



cleanEx()
nameEx("constantVector")
### * constantVector

flush(stderr()); flush(stdout())

### Name: constantVector
### Title: Create a constant vector object
### Aliases: constantVector

### ** Examples

# This function is not public.

# constantVector(0, 5)



cleanEx()
nameEx("constrainMatrices")
### * constrainMatrices

flush(stderr()); flush(stdout())

### Name: constrainMatrices
### Title: Impose an equality constraint in an object
### Aliases: constrainMatrices constrainMatrices-methods
###   constrainMatrices,ANY,ANY-method
###   constrainMatrices,MatrixSet,SimEqualCon-method

### ** Examples

# No example



cleanEx()
nameEx("continuousPower")
### * continuousPower

flush(stderr()); flush(stdout())

### Name: continuousPower
### Title: Find power of model parameters when simulations have randomly
###   varying parameters
### Aliases: continuousPower

### ** Examples

# Specify Sample Size by n
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- simMatrix(loading, 0.7)
RPH <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model <- simSetCFA(LY = LX, RPS = RPH, RTE = RTD)
SimData <- simData(CFA.Model, 500)
SimModel <- simModel(CFA.Model)
# We will use only 50 replications to save time.
# In reality, more replications are needed.

# Specify both sample size and percent missing completely at random
Output <- simResult(NULL, SimData, SimModel, n=seq(50, 500, 25), pmMCAR=c(0, 0.1, 0.2))
summary(Output)

Cpow <- continuousPower(Output, contN = TRUE, contMCAR = TRUE)
Cpow




cleanEx()
nameEx("countFreeParameters")
### * countFreeParameters

flush(stderr()); flush(stdout())

### Name: countFreeParameters
### Title: Count how many free parameters in the target object
### Aliases: countFreeParameters countFreeParameters-methods
###   countFreeParameters,ANY-method countFreeParameters,SimMatrix-method
###   countFreeParameters,SymMatrix-method
###   countFreeParameters,SimVector-method
###   countFreeParameters,SimSet-method countFreeParameters,matrix-method
###   countFreeParameters,vector-method
###   countFreeParameters,VirtualRSet-method
###   countFreeParameters,SimEqualCon-method
###   countFreeParameters,SimREqualCon-method

### ** Examples

# No example



cleanEx()
nameEx("countMACS")
### * countMACS

flush(stderr()); flush(stdout())

### Name: countMACS
### Title: Count the number of elements in the sufficient statistics
### Aliases: countMACS

### ** Examples

# No example



cleanEx()
nameEx("cov2corMod")
### * cov2corMod

flush(stderr()); flush(stdout())

### Name: cov2corMod
### Title: Convert a covariance matrix to a correlation matrix
### Aliases: cov2corMod

### ** Examples

# No example



cleanEx()
nameEx("createData")
### * createData

flush(stderr()); flush(stdout())

### Name: createData
### Title: Create data from model parameters
### Aliases: createData

### ** Examples

# No example



cleanEx()
nameEx("createFreeParameters")
### * createFreeParameters

flush(stderr()); flush(stdout())

### Name: createFreeParameters
### Title: Create a free parameters object from a model specification
### Aliases: createFreeParameters

### ** Examples

# No comment out because this function is not public

# loading <- matrix(0, 6, 2)
# loading[1:3, 1] <- NA
# loading[4:6, 2] <- NA
# loadingValues <- matrix(0, 6, 2)
# loadingValues[1:3, 1] <- 0.7
# loadingValues[4:6, 2] <- 0.7
# LX <- simMatrix(loading, loadingValues)
# latent.cor <- matrix(NA, 2, 2)
# diag(latent.cor) <- 1
# RPH <- symMatrix(latent.cor, 0.5)
# error.cor <- matrix(0, 6, 6)
# diag(error.cor) <- 1
# RTD <- symMatrix(error.cor)
# indicator.mean <- rep(NA, 6)
# MX <- simVector(indicator.mean, 0)
# CFA.Model <- simSetCFA(LX = LX, RPH = RPH, RTD = RTD, MX = MX)
# free <- createFreeParameters(CFA.Model)



cleanEx()
nameEx("createImpliedMACS")
### * createImpliedMACS

flush(stderr()); flush(stdout())

### Name: createImpliedMACS
### Title: Create model implied mean vector and covariance matrix
### Aliases: createImpliedMACS createImpliedMACS-methods
###   createImpliedMACS,MatrixSet-method createImpliedMACS,SimRSet-method

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- simMatrix(loading, loadingValues)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, 0.5)
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
RTD <- symMatrix(error.cor)
CFA.Model <- simSetCFA(LY = LX, RPS = RPH, RTE = RTD)
param <- run(CFA.Model)
createImpliedMACS(param)



cleanEx()
nameEx("defaultStartingValues")
### * defaultStartingValues

flush(stderr()); flush(stdout())

### Name: defaultStartingValues
### Title: Make ad hoc starting values
### Aliases: defaultStartingValues

### ** Examples

# No example



cleanEx()
nameEx("divideObject")
### * divideObject

flush(stderr()); flush(stdout())

### Name: divideObject
### Title: Make a division on each element of the object
### Aliases: divideObject divideObject-methods divideObject,ANY-method
###   divideObject,vector,numeric-method divideObject,matrix,numeric-method
###   divideObject,MatrixSet,numeric-method

### ** Examples

# No example



cleanEx()
nameEx("drawParameters")
### * drawParameters

flush(stderr()); flush(stdout())

### Name: drawParameters
### Title: Create parameter sets (with or without model misspecification)
###   from the data object
### Aliases: drawParameters

### ** Examples

# No example



cleanEx()
nameEx("drawParametersMisspec")
### * drawParametersMisspec

flush(stderr()); flush(stdout())

### Name: drawParametersMisspec
### Title: Create parameter sets (with or without model misspecification)
###   from the parameter with or without misspecification set
### Aliases: drawParametersMisspec

### ** Examples

# No example



cleanEx()
nameEx("expandMatrices")
### * expandMatrices

flush(stderr()); flush(stdout())

### Name: expandMatrices
### Title: Expand the set of intercept and covariance matrices into the set
###   of intercept/mean and covariance/correlation/variance objects
### Aliases: expandMatrices

### ** Examples

# No example



cleanEx()
nameEx("extract")
### * extract

flush(stderr()); flush(stdout())

### Name: extract
### Title: Extract a part of an object
### Aliases: extract extract-methods extract,vector-method
###   extract,matrix-method extract,VirtualRSet-method
###   extract,data.frame-method

### ** Examples

extract(1:10, c(4, 5))
extract(diag(3), 1, 2:3)



cleanEx()
nameEx("extractLavaanFit")
### * extractLavaanFit

flush(stderr()); flush(stdout())

### Name: extractLavaanFit
### Title: Extract fit indices from the lavaan object
### Aliases: extractLavaanFit

### ** Examples

# No example



cleanEx()
nameEx("extractMatrixNames")
### * extractMatrixNames

flush(stderr()); flush(stdout())

### Name: extractMatrixNames
### Title: Extract a vector of parameter names based on specified rows and
###   columns
### Aliases: extractMatrixNames

### ** Examples

# The function is not public

# vec <- c("LY1_1", "LY2_1", "LY3_1", "LY4_2", "LY5_2", "LY6_2", "LY7_3")
# extractMatrixNames(vec, 5:6, 2)



cleanEx()
nameEx("extractOpenMxFit")
### * extractOpenMxFit

flush(stderr()); flush(stdout())

### Name: extractOpenMxFit
### Title: Extract the fit indices reported by the 'OpenMx' result
### Aliases: extractOpenMxFit

### ** Examples

# No example



cleanEx()
nameEx("extractVectorNames")
### * extractVectorNames

flush(stderr()); flush(stdout())

### Name: extractVectorNames
### Title: Extract a vector of parameter names based on specified elements
### Aliases: extractVectorNames

### ** Examples

# The function is not public

#vec <- c("TY1", "TY2", "TY3", "TY4", "TY5", "TY6", "TY7")
#extractVectorNames(vec, 5:6)



cleanEx()
nameEx("fillParam")
### * fillParam

flush(stderr()); flush(stdout())

### Name: fillParam
### Title: Fill in other objects based on the parameter values of current
###   objects
### Aliases: fillParam

### ** Examples

# No example



cleanEx()
nameEx("findFactorIntercept")
### * findFactorIntercept

flush(stderr()); flush(stdout())

### Name: findFactorIntercept
### Title: Find factor intercept from regression coefficient matrix and
###   factor total means
### Aliases: findFactorIntercept

### ** Examples

path <- matrix(0, 9, 9)
path[4, 1] <- path[7, 4] <- 0.6
path[5, 2] <- path[8, 5] <- 0.6
path[6, 3] <- path[9, 6] <- 0.6
path[5, 1] <- path[8, 4] <- 0.4
path[6, 2] <- path[9, 5] <- 0.4
factorMean <- c(5, 2, 3, 0, 0, 0, 0, 0, 0)
findFactorIntercept(path, factorMean)



cleanEx()
nameEx("findFactorMean")
### * findFactorMean

flush(stderr()); flush(stdout())

### Name: findFactorMean
### Title: Find factor total means from regression coefficient matrix and
###   factor intercept
### Aliases: findFactorMean

### ** Examples

path <- matrix(0, 9, 9)
path[4, 1] <- path[7, 4] <- 0.6
path[5, 2] <- path[8, 5] <- 0.6
path[6, 3] <- path[9, 6] <- 0.6
path[5, 1] <- path[8, 4] <- 0.4
path[6, 2] <- path[9, 5] <- 0.4
intcept <- c(5, 2, 3, 0, 0, 0, 0, 0, 0)
findFactorMean(path, intcept)



cleanEx()
nameEx("findFactorResidualVar")
### * findFactorResidualVar

flush(stderr()); flush(stdout())

### Name: findFactorResidualVar
### Title: Find factor residual variances from regression coefficient
###   matrix, factor (residual) correlations, and total factor variances
### Aliases: findFactorResidualVar

### ** Examples

path <- matrix(0, 9, 9)
path[4, 1] <- path[7, 4] <- 0.6
path[5, 2] <- path[8, 5] <- 0.6
path[6, 3] <- path[9, 6] <- 0.6
path[5, 1] <- path[8, 4] <- 0.4
path[6, 2] <- path[9, 5] <- 0.4
facCor <- diag(9)
facCor[1, 2] <- facCor[2, 1] <- 0.4
facCor[1, 3] <- facCor[3, 1] <- 0.4
facCor[2, 3] <- facCor[3, 2] <- 0.4
totalVar <- rep(1, 9)
findFactorResidualVar(path, facCor, totalVar)



cleanEx()
nameEx("findFactorTotalCov")
### * findFactorTotalCov

flush(stderr()); flush(stdout())

### Name: findFactorTotalCov
### Title: Find factor total covariance from regression coefficient matrix,
###   factor residual covariance
### Aliases: findFactorTotalCov

### ** Examples

path <- matrix(0, 9, 9)
path[4, 1] <- path[7, 4] <- 0.6
path[5, 2] <- path[8, 5] <- 0.6
path[6, 3] <- path[9, 6] <- 0.6
path[5, 1] <- path[8, 4] <- 0.4
path[6, 2] <- path[9, 5] <- 0.4
facCor <- diag(9)
facCor[1, 2] <- facCor[2, 1] <- 0.4
facCor[1, 3] <- facCor[3, 1] <- 0.4
facCor[2, 3] <- facCor[3, 2] <- 0.4
residualVar <- c(1, 1, 1, 0.64, 0.288, 0.288, 0.64, 0.29568, 0.21888)
findFactorTotalCov(path, corPsi=facCor, errorVarPsi=residualVar)



cleanEx()
nameEx("findFactorTotalVar")
### * findFactorTotalVar

flush(stderr()); flush(stdout())

### Name: findFactorTotalVar
### Title: Find factor total variances from regression coefficient matrix,
###   factor (residual) correlations, and factor residual variances
### Aliases: findFactorTotalVar

### ** Examples

path <- matrix(0, 9, 9)
path[4, 1] <- path[7, 4] <- 0.6
path[5, 2] <- path[8, 5] <- 0.6
path[6, 3] <- path[9, 6] <- 0.6
path[5, 1] <- path[8, 4] <- 0.4
path[6, 2] <- path[9, 5] <- 0.4
facCor <- diag(9)
facCor[1, 2] <- facCor[2, 1] <- 0.4
facCor[1, 3] <- facCor[3, 1] <- 0.4
facCor[2, 3] <- facCor[3, 2] <- 0.4
residualVar <- c(1, 1, 1, 0.64, 0.288, 0.288, 0.64, 0.29568, 0.21888)
findFactorTotalVar(path, facCor, residualVar)



cleanEx()
nameEx("findIndIntercept")
### * findIndIntercept

flush(stderr()); flush(stdout())

### Name: findIndIntercept
### Title: Find indicator intercepts from factor loading matrix, total
###   factor mean, and indicator mean.
### Aliases: findIndIntercept

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- c(0.6, 0.7, 0.8)
loading[4:6, 2] <- c(0.6, 0.7, 0.8)
facMean <- c(0.5, 0.2)
indMean <- rep(1, 6)
findIndIntercept(loading, facMean, indMean)



cleanEx()
nameEx("findIndMean")
### * findIndMean

flush(stderr()); flush(stdout())

### Name: findIndMean
### Title: Find indicator total means from factor loading matrix, total
###   factor mean, and indicator intercept.
### Aliases: findIndMean

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- c(0.6, 0.7, 0.8)
loading[4:6, 2] <- c(0.6, 0.7, 0.8)
facMean <- c(0.5, 0.2)
intcept <- rep(0, 6)
findIndMean(loading, facMean, intcept)



cleanEx()
nameEx("findIndResidualVar")
### * findIndResidualVar

flush(stderr()); flush(stdout())

### Name: findIndResidualVar
### Title: Find indicator residual variances from factor loading matrix,
###   total factor covariance, and total indicator variances.
### Aliases: findIndResidualVar

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- c(0.6, 0.7, 0.8)
loading[4:6, 2] <- c(0.6, 0.7, 0.8)
facCov <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
totalVar <- rep(1, 6)
findIndResidualVar(loading, facCov, totalVar)



cleanEx()
nameEx("findIndTotalVar")
### * findIndTotalVar

flush(stderr()); flush(stdout())

### Name: findIndTotalVar
### Title: Find indicator total variances from factor loading matrix, total
###   factor covariance, and indicator residual variances.
### Aliases: findIndTotalVar

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- c(0.6, 0.7, 0.8)
loading[4:6, 2] <- c(0.6, 0.7, 0.8)
facCov <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
resVar <- c(0.64, 0.51, 0.36, 0.64, 0.51, 0.36)
findIndTotalVar(loading, facCov, resVar)



cleanEx()
nameEx("findPossibleFactorCor")
### * findPossibleFactorCor

flush(stderr()); flush(stdout())

### Name: findPossibleFactorCor
### Title: Find the appropriate position for freely estimated correlation
###   (or covariance) given a regression coefficient matrix
### Aliases: findPossibleFactorCor

### ** Examples

path <- matrix(0, 9, 9)
path[4, 1] <- path[7, 4] <- NA
path[5, 2] <- path[8, 5] <- NA
path[6, 3] <- path[9, 6] <- NA
path[5, 1] <- path[8, 4] <- NA
path[6, 2] <- path[9, 5] <- NA
findPossibleFactorCor(path)



cleanEx()
nameEx("findRecursiveSet")
### * findRecursiveSet

flush(stderr()); flush(stdout())

### Name: findRecursiveSet
### Title: Group variables regarding the position in mediation chain
### Aliases: findRecursiveSet

### ** Examples

path <- matrix(0, 9, 9)
path[4, 1] <- path[7, 4] <- NA
path[5, 2] <- path[8, 5] <- NA
path[6, 3] <- path[9, 6] <- NA
path[5, 1] <- path[8, 4] <- NA
path[6, 2] <- path[9, 5] <- NA
findRecursiveSet(path)



cleanEx()
nameEx("findRowZero")
### * findRowZero

flush(stderr()); flush(stdout())

### Name: findRowZero
### Title: Find rows in a matrix that all elements are zero in non-fixed
###   subset rows and columns.
### Aliases: findRowZero

### ** Examples

# No example



cleanEx()
nameEx("fitMeasuresChi")
### * fitMeasuresChi

flush(stderr()); flush(stdout())

### Name: fitMeasuresChi
### Title: Find fit indices from the discrepancy values of the target model
###   and null models.
### Aliases: fitMeasuresChi

### ** Examples

# No example



cleanEx()
nameEx("freeVector")
### * freeVector

flush(stderr()); flush(stdout())

### Name: freeVector
### Title: Create a free parameters vector with a starting values in a
###   vector object
### Aliases: freeVector

### ** Examples

# This function is not a public function.

# freeVector(0, 5)



cleanEx()
nameEx("getCutoff")
### * getCutoff

flush(stderr()); flush(stdout())

### Name: getCutoff
### Title: Find cutoff given a priori alpha level
### Aliases: getCutoff getCutoff-methods getCutoff,data.frame-method
###   getCutoff,matrix-method getCutoff,SimResult-method

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- simMatrix(loading, loadingValues)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, 0.5)
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
RTD <- symMatrix(error.cor)
CFA.Model <- simSetCFA(LY = LX, RPS = RPH, RTE = RTD)
SimData <- simData(CFA.Model, 200)
SimModel <- simModel(CFA.Model)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
Output <- simResult(50, SimData, SimModel)
getCutoff(Output, 0.05)



cleanEx()
nameEx("getKeywords")
### * getKeywords

flush(stderr()); flush(stdout())

### Name: getKeywords
### Title: List of all keywords used in the 'simsem' package
### Aliases: getKeywords

### ** Examples

# This function is not a public function.

# getKeywords()



cleanEx()
nameEx("getPopulation")
### * getPopulation

flush(stderr()); flush(stdout())

### Name: getPopulation
### Title: Extract the data generation population model underlying an
###   object
### Aliases: getPopulation getPopulation-methods getPopulation,ANY-method

### ** Examples

# See each class for an example.



cleanEx()
nameEx("getPower")
### * getPower

flush(stderr()); flush(stdout())

### Name: getPower
### Title: Find power in rejecting alternative models based on fit indices
###   criteria
### Aliases: getPower getPower-methods getPower,data.frame-method
###   getPower,matrix-method getPower,SimResult-method

### ** Examples

loading.null <- matrix(0, 6, 1)
loading.null[1:6, 1] <- NA
LX.NULL <- simMatrix(loading.null, 0.7)
RPH.NULL <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model.NULL <- simSetCFA(LY = LX.NULL, RPS = RPH.NULL, RTE = RTD)
SimData.NULL <- simData(CFA.Model.NULL, 500)
SimModel <- simModel(CFA.Model.NULL)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
Output.NULL <- simResult(50, SimData.NULL, SimModel)
Cut.NULL <- getCutoff(Output.NULL, 0.95)

u79 <- simUnif(0.7, 0.9)
loading.alt <- matrix(0, 6, 2)
loading.alt[1:3, 1] <- NA
loading.alt[4:6, 2] <- NA
LX.ALT <- simMatrix(loading.alt, 0.7)
latent.cor.alt <- matrix(NA, 2, 2)
diag(latent.cor.alt) <- 1
RPH.ALT <- symMatrix(latent.cor.alt, "u79")
CFA.Model.ALT <- simSetCFA(LY = LX.ALT, RPS = RPH.ALT, RTE = RTD)
SimData.ALT <- simData(CFA.Model.ALT, 500)
Output.ALT <- simResult(50, SimData.ALT, SimModel)
getPower(Output.ALT, Cut.NULL)
Rule.of.thumb <- c(RMSEA=0.05, CFI=0.95, TLI=0.95, SRMR=0.06)
getPower(Output.ALT, Rule.of.thumb, usedFit=c("RMSEA", "CFI", "TLI", "SRMR"))



cleanEx()
nameEx("imposeMissing")
### * imposeMissing

flush(stderr()); flush(stdout())

### Name: imposeMissing
### Title: Impose MAR, MCAR, planned missingness, or attrition on a data
###   set
### Aliases: imposeMissing

### ** Examples

  data <- matrix(rep(rnorm(10,1,1),19),ncol=19)
  datac <- cbind(data,rnorm(10,0,1),rnorm(10,5,5))
 
  # Imposing Missing with the following arguments produces no missing values
  imposeMissing(data)
  imposeMissing(data,cov=c(1,2))
  imposeMissing(data,pmMCAR=0)
  imposeMissing(data,pmMAR=0)
  imposeMissing(data,nforms=0)

  #Some more usage examples
  imposeMissing(data,cov=c(1,2),pmMCAR=.1)
  
 
  imposeMissing(data,nforms=3)
  imposeMissing(data,nforms=3,itemGroups=list(c(1,2,3,4,5),c(6,7,8,9,10),c(11,12,13,14,15),c(16,17,18,19)))
  imposeMissing(datac,cov=c(20,21),nforms=3)
  imposeMissing(data,twoMethod=c(19,.8))
  imposeMissing(datac,cov=21,prAttr=.1,timePoints=5)




cleanEx()
nameEx("indProd")
### * indProd

flush(stderr()); flush(stdout())

### Name: indProd
### Title: Make a product of indicators using mean centering or double-mean
###   centering
### Aliases: indProd

### ** Examples

dat <- indProd(attitude[,-1], var1=1:3, var2=4:6)



cleanEx()
nameEx("isCorMatrix")
### * isCorMatrix

flush(stderr()); flush(stdout())

### Name: isCorMatrix
### Title: Check whether a 'matrix' is a possible correlation matrix
### Aliases: isCorMatrix

### ** Examples

# This function is not a public function.

# isCorMatrix(diag(5))



cleanEx()
nameEx("isDefault")
### * isDefault

flush(stderr()); flush(stdout())

### Name: isDefault
### Title: Check whether a vector object is default
### Aliases: isDefault

### ** Examples

# No example



cleanEx()
nameEx("isMeanConstraint")
### * isMeanConstraint

flush(stderr()); flush(stdout())

### Name: isMeanConstraint
### Title: Check whether all rownames in a constraint matrix containing
###   symbols of means vectors
### Aliases: isMeanConstraint

### ** Examples

# No example



cleanEx()
nameEx("isNullObject")
### * isNullObject

flush(stderr()); flush(stdout())

### Name: isNullObject
### Title: Check whether the object is the 'NULL' type of that class
### Aliases: isNullObject isNullObject-methods isNullObject,ANY,ANY-method
###   isNullObject,vector-method isNullObject,matrix-method
###   isNullObject,SimMatrix-method isNullObject,SymMatrix-method
###   isNullObject,SimVector-method isNullObject,SimSet-method
###   isNullObject,SimEqualCon-method isNullObject,SimREqualCon-method
###   isNullObject,SimMisspec-method isNullObject,VirtualRSet-method
###   isNullObject,data.frame-method isNullObject,SimMissing-method
###   isNullObject,SimDataDist-method isNullObject,SimFunction-method

### ** Examples

# No example



cleanEx()
nameEx("isRandom")
### * isRandom

flush(stderr()); flush(stdout())

### Name: isRandom
### Title: Check whether the object contains any random parameters
### Aliases: isRandom isRandom-methods isRandom,ANY-method
###   isRandom,SimMatrix-method isRandom,SimVector-method
###   isRandom,SimSet-method

### ** Examples

# No example



cleanEx()
nameEx("isVarianceConstraint")
### * isVarianceConstraint

flush(stderr()); flush(stdout())

### Name: isVarianceConstraint
### Title: Check whether all rownames in a constraint matrix containing
###   symbols of variance vectors
### Aliases: isVarianceConstraint

### ** Examples

# No example



cleanEx()
nameEx("kStat")
### * kStat

flush(stderr()); flush(stdout())

### Name: kStat
### Title: Calculate the _k_-statistic of a variable
### Aliases: kStat

### ** Examples

# This function is not a public function.

# kStat(1:5, 4)



cleanEx()
nameEx("kurtosis")
### * kurtosis

flush(stderr()); flush(stdout())

### Name: kurtosis
### Title: Finding excessive kurtosis
### Aliases: kurtosis kurtosis-methods kurtosis,vector-method

### ** Examples

kurtosis(1:5)



cleanEx()
nameEx("loadingFromAlpha")
### * loadingFromAlpha

flush(stderr()); flush(stdout())

### Name: loadingFromAlpha
### Title: Find standardized factor loading from coefficient alpha
### Aliases: loadingFromAlpha

### ** Examples

    loadingFromAlpha(0.8, 4)



cleanEx()
nameEx("makeLabels")
### * makeLabels

flush(stderr()); flush(stdout())

### Name: makeLabels
### Title: Make parameter names for each element in matrices or vectors or
###   the name for the whole object
### Aliases: makeLabels makeLabels-methods makeLabels,ANY-method
###   makeLabels,vector-method makeLabels,matrix-method
###   makeLabels,SimParam-method makeLabels,VirtualDist-method
###   makeLabels,SimSet-method

### ** Examples

# No example



cleanEx()
nameEx("matchKeywords")
### * matchKeywords

flush(stderr()); flush(stdout())

### Name: matchKeywords
### Title: Search for the keywords and check whether the specified text
###   match one in the name vector
### Aliases: matchKeywords

### ** Examples

# This function is not a public function.

# matchKeywords("ly", c("LY", "LX"))



cleanEx()
nameEx("miPool")
### * miPool

flush(stderr()); flush(stdout())

### Name: miPool
### Title: Function to pool imputed results
### Aliases: miPool

### ** Examples

# No Example



cleanEx()
nameEx("miPoolChi")
### * miPoolChi

flush(stderr()); flush(stdout())

### Name: miPoolChi
### Title: Function to pool chi-square statistics from the result from
###   multiple imputation
### Aliases: miPoolChi

### ** Examples

miPoolChi(c(89.864, 81.116, 71.500, 49.022, 61.986, 64.422, 55.256, 57.890, 79.416, 63.944), 2)



cleanEx()
nameEx("miPoolVector")
### * miPoolVector

flush(stderr()); flush(stdout())

### Name: miPoolVector
### Title: Function to pool imputed results that saved in a matrix format
### Aliases: miPoolVector

### ** Examples

param <- matrix(c(0.7, 0.1, 0.5,
					0.75, 0.12, 0.54,
					0.66, 0.11, 0.56,
					0.74, 0.09, 0.55), nrow=4, byrow=TRUE)
SE <- matrix(c(0.1, 0.01, 0.05,
				0.11, 0.023, 0.055,
				0.10, 0.005, 0.04,
				0.14, 0.012, 0.039), nrow=4, byrow=TRUE)
nimps <- 4
miPoolVector(param, SE, nimps)



cleanEx()
nameEx("overlapHist")
### * overlapHist

flush(stderr()); flush(stdout())

### Name: overlapHist
### Title: Plot overlapping histograms
### Aliases: overlapHist

### ** Examples

# This function is not a public function.

# a <- rnorm(10000, 0, 1)
# b <- rnorm(10000, 1, 1.5)
# overlapHist(a, b, main="Example")



cleanEx()
nameEx("pValue")
### * pValue

flush(stderr()); flush(stdout())

### Name: pValue
### Title: Find p-values (1 - percentile)
### Aliases: pValue pValue-methods pValue,ANY-method
###   pValue,numeric,vector-method pValue,numeric,data.frame-method
###   pValue,SimModelOut,SimResult-method

### ** Examples

# Compare number with a vector
pValue(0.5, rnorm(1000, 0, 1))

# Compare numbers with a data frame
pValue(c(0.5, 0.2), data.frame(rnorm(1000, 0, 1), runif(1000, 0, 1)))

# Compare an analysis result with a result of simulation study
library(lavaan)
loading <- matrix(0, 9, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
model <- simParamCFA(LY=loading)
SimModel <- simModel(model, indLab=paste("x", 1:9, sep=""))
u2 <- simUnif(-0.2, 0.2)
loading.trivial <- matrix(NA, 9, 3)
loading.trivial[is.na(loading)] <- 0
LY.trivial <- simMatrix(loading.trivial, "u2")
mis <- simMisspecCFA(LY = LY.trivial)
out <- run(SimModel, HolzingerSwineford1939)
Output2 <- runFit(out, HolzingerSwineford1939, 20, mis)
pValue(out, Output2)



cleanEx()
nameEx("plot3DQtile")
### * plot3DQtile

flush(stderr()); flush(stdout())

### Name: plot3DQtile
### Title: Build a persepctive plot or contour plot of a quantile of
###   predicted values
### Aliases: plot3DQtile

### ** Examples

# No example



cleanEx()
nameEx("plotCutoff")
### * plotCutoff

flush(stderr()); flush(stdout())

### Name: plotCutoff
### Title: Plot sampling distributions of fit indices
### Aliases: plotCutoff plotCutoff-methods plotCutoff,data.frame-method
###   plotCutoff,SimResult-method

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- simMatrix(loading, loadingValues)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, 0.5)
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
RTD <- symMatrix(error.cor)
CFA.Model <- simSetCFA(LY = LX, RPS = RPH, RTE = RTD)
SimData <- simData(CFA.Model, 200)
SimModel <- simModel(CFA.Model)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
Output <- simResult(50, SimData, SimModel)
plotCutoff(Output, 0.05, usedFit=c("RMSEA", "SRMR", "CFI", "TLI"))

# Varying N
Output2 <- simResult(NULL, SimData, SimModel, n=51:100)
plotCutoff(Output2, 0.05)

# Varying N and pmMCAR
Output3 <- simResult(NULL, SimData, SimModel, n=51:100, pmMCAR=c(0, 0.05, 0.1, 0.15))
plotCutoff(Output3, 0.05)



cleanEx()
nameEx("plotDist")
### * plotDist

flush(stderr()); flush(stdout())

### Name: plotDist
### Title: Plot a distribution of a distribution object or data
###   distribution object
### Aliases: plotDist plotDist-methods

### ** Examples

gamma11 <- simGamma(1, 1)
plotDist(gamma11)

chi <- simChisq(5)
dataDist <- simDataDist(chi, chi)
plotDist(dataDist)



cleanEx()
nameEx("plotMisfit")
### * plotMisfit

flush(stderr()); flush(stdout())

### Name: plotMisfit
### Title: Plot the population misfit in parameter result object
### Aliases: plotMisfit

### ** Examples

u35 <- simUnif(0.3, 0.5)
u57 <- simUnif(0.5, 0.7)
u1 <- simUnif(-0.1, 0.1)
n31 <- simNorm(0.3, 0.1)

path.BE <- matrix(0, 4, 4)
path.BE[3, 1:2] <- NA
path.BE[4, 3] <- NA
starting.BE <- matrix("", 4, 4)
starting.BE[3, 1:2] <- "u35"
starting.BE[4, 3] <- "u57"
BE <- simMatrix(path.BE, starting.BE)

residual.error <- diag(4)
residual.error[1,2] <- residual.error[2,1] <- NA
RPS <- symMatrix(residual.error, "n31")

ME <- simVector(rep(NA, 4), 0)

Path.Model <- simSetPath(RPS = RPS, BE = BE, ME = ME)

mis.path.BE <- matrix(0, 4, 4)
mis.path.BE[4, 1:2] <- NA
mis.BE <- simMatrix(mis.path.BE, "u1")
Path.Mis.Model <- simMisspecPath(BE = mis.BE, misfitType="rmsea") #, misfitBound=c(0.05, 0.08))

# The number of replications in actual analysis should be much more than 50
ParamObject <- simResultParam(50, Path.Model, Path.Mis.Model)
plotMisfit(ParamObject)

plotMisfit(ParamObject, misParam=1:2)



cleanEx()
nameEx("plotPower")
### * plotPower

flush(stderr()); flush(stdout())

### Name: plotPower
### Title: Plot sampling distributions of fit indices that visualize power
### Aliases: plotPower plotPower-methods
###   plotPower,data.frame,data.frame-method
###   plotPower,data.frame,vector-method
###   plotPower,SimResult,SimResult-method
###   plotPower,SimResult,vector-method

### ** Examples

loading.null <- matrix(0, 6, 1)
loading.null[1:6, 1] <- NA
LX.NULL <- simMatrix(loading.null, 0.7)
RPH.NULL <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model.NULL <- simSetCFA(LY = LX.NULL, RPS = RPH.NULL, RTE = RTD)
SimData.NULL <- simData(CFA.Model.NULL, 500)
SimModel <- simModel(CFA.Model.NULL)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
Output.NULL <- simResult(50, SimData.NULL, SimModel)
Cut.NULL <- getCutoff(Output.NULL, 0.95)

u79 <- simUnif(0.7, 0.9)
loading.alt <- matrix(0, 6, 2)
loading.alt[1:3, 1] <- NA
loading.alt[4:6, 2] <- NA
LX.ALT <- simMatrix(loading.alt, 0.7)
latent.cor.alt <- matrix(NA, 2, 2)
diag(latent.cor.alt) <- 1
RPH.ALT <- symMatrix(latent.cor.alt, "u79")
CFA.Model.ALT <- simSetCFA(LY = LX.ALT, RPS = RPH.ALT, RTE = RTD)
SimData.ALT <- simData(CFA.Model.ALT, 500)
Output.ALT <- simResult(50, SimData.ALT, SimModel)
getPower(Output.ALT, Cut.NULL)
Rule.of.thumb <- c(RMSEA=0.05, CFI=0.95, TLI=0.95, SRMR=0.06)
plotPower(Output.ALT, Output.NULL, alpha=0.05, usedFit=c("RMSEA", "CFI", "TLI", "SRMR"))



cleanEx()
nameEx("plotQtile")
### * plotQtile

flush(stderr()); flush(stdout())

### Name: plotQtile
### Title: Build a scatterplot with overlaying line of quantiles of
###   predicted values
### Aliases: plotQtile

### ** Examples

# No example



cleanEx()
nameEx("popDiscrepancy")
### * popDiscrepancy

flush(stderr()); flush(stdout())

### Name: popDiscrepancy
### Title: Find the discrepancy value between two means and covariance
###   matrices
### Aliases: popDiscrepancy

### ** Examples

m1 <- rep(0, 3)
m2 <- c(0.1, -0.1, 0.05)
S1 <- matrix(c(1, 0.6, 0.5, 0.6, 1, 0.4, 0.5, 0.4, 1), 3, 3)
S2 <- matrix(c(1, 0.55, 0.55, 0.55, 1, 0.55, 0.55, 0.55, 1), 3, 3)
popDiscrepancy(m1, S1, m2, S2)



cleanEx()
nameEx("popMisfit")
### * popMisfit

flush(stderr()); flush(stdout())

### Name: popMisfit
### Title: Calculate population misfit
### Aliases: popMisfit popMisfit-methods popMisfit,ANY,ANY-method
###   popMisfit,matrix,matrix-method popMisfit,list,list-method
###   popMisfit,SimRSet,SimRSet-method popMisfit,MatrixSet,MatrixSet-method
###   popMisfit,SimSet,SimMisspec-method

### ** Examples

u35 <- simUnif(0.3, 0.5)
u57 <- simUnif(0.5, 0.7)
u1 <- simUnif(-0.1, 0.1)
n31 <- simNorm(0.3, 0.1)

path.BE <- matrix(0, 4, 4)
path.BE[3, 1:2] <- NA
path.BE[4, 3] <- NA
starting.BE <- matrix("", 4, 4)
starting.BE[3, 1:2] <- "u35"
starting.BE[4, 3] <- "u57"
BE <- simMatrix(path.BE, starting.BE)

residual.error <- diag(4)
residual.error[1,2] <- residual.error[2,1] <- NA
RPS <- symMatrix(residual.error, "n31")

ME <- simVector(rep(NA, 4), 0)

Path.Model <- simSetPath(RPS = RPS, BE = BE, ME = ME)

mis.path.BE <- matrix(0, 4, 4)
mis.path.BE[4, 1:2] <- NA
mis.BE <- simMatrix(mis.path.BE, "u1")
Path.Mis.Model <- simMisspecPath(BE = mis.BE, misfitType="rmsea") #, misfitBound=c(0.05, 0.08))

popMisfit(Path.Model, Path.Mis.Model, fit.measures="rmsea")



cleanEx()
nameEx("popMisfitMACS")
### * popMisfitMACS

flush(stderr()); flush(stdout())

### Name: popMisfitMACS
### Title: Find population misfit by sufficient statistics
### Aliases: popMisfitMACS

### ** Examples

m1 <- rep(0, 3)
m2 <- c(0.1, -0.1, 0.05)
S1 <- matrix(c(1, 0.6, 0.5, 0.6, 1, 0.4, 0.5, 0.4, 1), 3, 3)
S2 <- matrix(c(1, 0.55, 0.55, 0.55, 1, 0.55, 0.55, 0.55, 1), 3, 3)
popMisfitMACS(m1, S1, m2, S2)



cleanEx()
nameEx("printIfNotNull")
### * printIfNotNull

flush(stderr()); flush(stdout())

### Name: printIfNotNull
### Title: Provide basic summary of each object if that object is not NULL.
### Aliases: printIfNotNull

### ** Examples

# This function is not public

# AL <- simVector(rep(NA, 5), "0")
# printIfNotNull(AL, "Factor mean")



cleanEx()
nameEx("reassignNames")
### * reassignNames

flush(stderr()); flush(stdout())

### Name: reassignNames
### Title: Reassign the name of equality constraint
### Aliases: reassignNames

### ** Examples

# No example



cleanEx()
nameEx("reduceConstraint")
### * reduceConstraint

flush(stderr()); flush(stdout())

### Name: reduceConstraint
### Title: Reduce the model constraint to data generation parameterization
###   to analysis model parameterization.
### Aliases: reduceConstraint

### ** Examples

# No example



cleanEx()
nameEx("reduceMatrices")
### * reduceMatrices

flush(stderr()); flush(stdout())

### Name: reduceMatrices
### Title: Reduce the model constraint to data generation parameterization
###   to analysis model parameterization.
### Aliases: reduceMatrices

### ** Examples

# No example



cleanEx()
nameEx("residualCovariate")
### * residualCovariate

flush(stderr()); flush(stdout())

### Name: residualCovariate
### Title: Residual centered all target indicators by covariates
### Aliases: residualCovariate

### ** Examples

dat <- residualCovariate(attitude, 2:7, 1)



cleanEx()
nameEx("run")
### * run

flush(stderr()); flush(stdout())

### Name: run
### Title: Run a particular object in 'simsem' package.
### Aliases: run run-methods run,ANY-method run,NullSimMatrix-method
###   run,NullSymMatrix-method run,NullSimVector-method
### Keywords: run

### ** Examples

n02 <- simNorm(0, 0.2)
run(n02)



cleanEx()
nameEx("runFit")
### * runFit

flush(stderr()); flush(stdout())

### Name: runFit
### Title: Build a result object from analyzing real data
### Aliases: runFit runFit-methods runFit,ANY-method runFit,SimModel-method
###   runFit,SimModelOut-method

### ** Examples

library(lavaan)
loading <- matrix(0, 9, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
model <- simParamCFA(LY=loading)
SimModel <- simModel(model, indLab=paste("x", 1:9, sep=""))
u2 <- simUnif(-0.2, 0.2)
loading.trivial <- matrix(NA, 9, 3)
loading.trivial[is.na(loading)] <- 0
LY.trivial <- simMatrix(loading.trivial, "u2")
mis <- simMisspecCFA(LY = LY.trivial)
Output <- runFit(SimModel, HolzingerSwineford1939, 20, mis)
summary(Output)

out <- run(SimModel, HolzingerSwineford1939)
Output2 <- runFit(out, HolzingerSwineford1939, 20, mis)

# Bollen-Stine Bootstrap
Output3 <- runFit(out, HolzingerSwineford1939, 20, modelBoot=TRUE)

# Bollen-Stine Bootstrap with trivial misspecification
Output4 <- runFit(out, HolzingerSwineford1939, 20, mis, modelBoot=TRUE)

# Example with multiple imputation
library(lavaan)
loading <- matrix(0, 11, 3)
loading[1:3, 1] <- NA
loading[4:7, 2] <- NA
loading[8:11, 3] <- NA
path <- matrix(0, 3, 3)
path[2:3, 1] <- NA
path[3, 2] <- NA
errorCov <- diag(NA, 11)
facCov <- diag(3)
param <- simParamSEM(LY=loading, BE=path, TE=errorCov, PS=facCov)

miss <- simMissing(pmMCAR=0.03, numImps=5)
usedData <- run(miss, PoliticalDemocracy)

model <- simModel(param, indLab=c(paste("x", 1:3, sep=""), paste("y", 1:8, sep="")))
out <- run(model, usedData, miss)
output <- runFit(model, usedData, 5, missModel=miss)
pValue(out, output)



cleanEx()
nameEx("runLavaan")
### * runLavaan

flush(stderr()); flush(stdout())

### Name: runLavaan
### Title: Run data by the model object by the 'lavaan' package
### Aliases: runLavaan

### ** Examples

# No example



cleanEx()
nameEx("runMI")
### * runMI

flush(stderr()); flush(stdout())

### Name: runMI
### Title: Multiply impute and analyze data using lavaan
### Aliases: runMI

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(data.mat,data.model,imps) {
  #Impute missing data
  imputed.l<-imputeMissing(data.mat,imps)
  
  #Run models on each imputed data set
  #Does this give results from each dataset in the list?
  
  imputed.results<-result.object(imputed.l[[1]],sim.data.model,10)

  imputed.results <- lapply(imputed.l,result.object,data.model,1)
  comb.results<-MIpool(imputed.results,imps)
  
  return(comb.results)

  }



cleanEx()
nameEx("runMisspec")
### * runMisspec

flush(stderr()); flush(stdout())

### Name: runMisspec
### Title: Draw actual parameters and model misspecification
### Aliases: runMisspec

### ** Examples

# No example



cleanEx()
nameEx("runRep")
### * runRep

flush(stderr()); flush(stdout())

### Name: runRep
### Title: Run one replication within a big simulation study
### Aliases: runRep

### ** Examples

# No example



cleanEx()
nameEx("setOpenMxObject")
### * setOpenMxObject

flush(stderr()); flush(stdout())

### Name: setOpenMxObject
### Title: Rearrange starting values for 'OpenMx'
### Aliases: setOpenMxObject setOpenMxObject-methods
###   setOpenMxObject,ANY,ANY-method setOpenMxObject,vector,vector-method
###   setOpenMxObject,matrix,matrix-method
###   setOpenMxObject,SimParam,SimRSet-method

### ** Examples

# This function is not public

# parameter <- c(NA, NA, 0, 0)
# startingValues <- c(2, 5, 0, 0)
# setOpenMxObject(parameter, startingValues)



cleanEx()
nameEx("setPopulation")
### * setPopulation

flush(stderr()); flush(stdout())

### Name: setPopulation
### Title: Set the data generation population model underlying an object
### Aliases: setPopulation setPopulation-methods setPopulation,ANY-method

### ** Examples

# See each class for an example.



cleanEx()
nameEx("simBeta")
### * simBeta

flush(stderr()); flush(stdout())

### Name: simBeta
### Title: Create random beta distribution object
### Aliases: simBeta

### ** Examples

    b11 <- simBeta(1, 1)
    run(b11)



cleanEx()
nameEx("simBinom")
### * simBinom

flush(stderr()); flush(stdout())

### Name: simBinom
### Title: Create random binomial distribution object
### Aliases: simBinom

### ** Examples

    b55 <- simBinom(5, 0.5)
    run(b55)
	summary(b55)



cleanEx()
nameEx("simCauchy")
### * simCauchy

flush(stderr()); flush(stdout())

### Name: simCauchy
### Title: Create random Cauchy distribution object
### Aliases: simCauchy

### ** Examples

    c02 <- simCauchy(0, 2)
    run(c02)
	summary(c02)



cleanEx()
nameEx("simChisq")
### * simChisq

flush(stderr()); flush(stdout())

### Name: simChisq
### Title: Create random chi-squared distribution object
### Aliases: simChisq

### ** Examples

    chi5 <- simChisq(5)
    run(chi5)
	summary(chi5)



cleanEx()
nameEx("simData")
### * simData

flush(stderr()); flush(stdout())

### Name: simData
### Title: Data object
### Aliases: simData simData-methods simData,ANY-method
###   simData,SimSet-method simData,SimModelOut-method
###   simData,SimRSet-method

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- simMatrix(loading, loadingValues)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, 0.5)
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
RTD <- symMatrix(error.cor)
CFA.Model <- simSetCFA(LY = LX, RPS = RPH, RTE = RTD)
SimData <- simData(CFA.Model, 200)
summary(SimData)
run(SimData)

# With Misspecification Model
n01 <- simNorm(0, 0.1)
error.cor.Mis <- matrix(NA, 6, 6)
diag(error.cor.Mis) <- 1
RTD.Mis <- symMatrix(error.cor.Mis, "n01")
CFA.Model.Mis <- simMisspecCFA(RTD=RTD.Mis)
SimData <- simData(CFA.Model, 200, misspec=CFA.Model.Mis)
summary(SimData)
run(SimData)



cleanEx()
nameEx("simDataDist")
### * simDataDist

flush(stderr()); flush(stdout())

### Name: simDataDist
### Title: Create a data distribution object.
### Aliases: simDataDist

### ** Examples

loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- simMatrix(loading, 0.7)
RPH <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model <- simSetCFA(LY = LX, RPS = RPH, RTE = RTD)
SimData <- simData(CFA.Model, 500)
SimModel <- simModel(CFA.Model)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
Output <- simResult(50, SimData, SimModel)
#summary(Output)



cleanEx()
nameEx("simEqualCon")
### * simEqualCon

flush(stderr()); flush(stdout())

### Name: simEqualCon
### Title: Equality Constraint Object
### Aliases: simEqualCon

### ** Examples

# Example 1: Single-group, one constraint
constraint <- matrix(0, 3, 2)
constraint[1,] <- c(1, 1)
constraint[2,] <- c(2, 1)
constraint[3,] <- c(3, 1)
rownames(constraint) <- rep("LY", 3)
equal.loading <- simEqualCon(constraint, modelType="SEM.exo")

# Example 2: Multiple-group, one constraint
group.con <- matrix(0, 2, 3)
group.con[1,] <- c(1, 2, 1)
group.con[2,] <- c(2, 2, 1)
rownames(group.con) <- rep("BE", 2)
equal.path <- simEqualCon(group.con, modelType="Path")

# Example 3: Single-group, multiple constraints
constraint1 <- matrix(1, 3, 2)
constraint1[,1] <- 1:3
rownames(constraint1) <- rep("LY", 3)
constraint2 <- matrix(2, 3, 2)
constraint2[,1] <- 4:6
rownames(constraint2) <- rep("LY", 3)
constraint3 <- matrix(3, 2, 2)
constraint3[,1] <- 7:8
rownames(constraint3) <- rep("LY", 2)
equal.loading2 <- simEqualCon(constraint1, constraint2, constraint3, modelType="SEM")
summary(equal.loading2)



cleanEx()
nameEx("simExp")
### * simExp

flush(stderr()); flush(stdout())

### Name: simExp
### Title: Create random exponential distribution object
### Aliases: simExp

### ** Examples

    exp2 <- simExp(2)
    run(exp2)
	summary(exp2)



cleanEx()
nameEx("simF")
### * simF

flush(stderr()); flush(stdout())

### Name: simF
### Title: Create random F distribution object
### Aliases: simF

### ** Examples

    f27 <- simF(2, 7)
    run(f27)
	summary(f27)



cleanEx()
nameEx("simFunction")
### * simFunction

flush(stderr()); flush(stdout())

### Name: simFunction
### Title: Create function object
### Aliases: simFunction

### ** Examples


n65 <- simNorm(0.6, 0.05)
u35 <- simUnif(0.3, 0.5)
u68 <- simUnif(0.6, 0.8)
u2 <- simUnif(-0.2, 0.2)
n1 <- simNorm(0, 0.1)

loading <- matrix(0, 9, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
loading.start <- matrix("", 9, 3)
loading.start[1:3, 1] <- 0.7
loading.start[4:6, 2] <- 0.7
loading.start[7:9, 3] <- "u68"
LY <- simMatrix(loading, loading.start)

RTE <- symMatrix(diag(9))

factor.cor <- diag(3)
factor.cor[1, 2] <- factor.cor[2, 1] <- NA
RPS <- symMatrix(factor.cor, 0.5)

path <- matrix(0, 3, 3)
path[3, 1:2] <- NA
path.start <- matrix(0, 3, 3)
path.start[3, 1] <- "n65"
path.start[3, 2] <- "u35"
BE <- simMatrix(path, path.start)

datGen <- simSetSEM(BE=BE, LY=LY, RPS=RPS, RTE=RTE)

loading.trivial <- matrix(NA, 9, 3)
loading.trivial[is.na(loading)] <- 0
LY.trivial <- simMatrix(loading.trivial, "u2")

error.cor.trivial <- matrix(NA, 9, 9)
diag(error.cor.trivial) <- 0
RTE.trivial <- symMatrix(error.cor.trivial, "n1")

misGen <- simMisspecSEM(LY = LY.trivial, RTE = RTE.trivial)

Data.Mis <- simData(datGen, 300, misspec=misGen)

loading <- matrix(0, 12, 4)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 4] <- NA
loading[10:12, 3] <- NA

path <- matrix(0, 4, 4)
path[4, 1:3] <- NA

analysis <- simParamSEM(BE=path, LY=loading)

Model <- simModel(analysis)

fun <- simFunction(indProd, var1=paste("y", 1:3, sep=""), var2=paste("y", 4:6, sep=""), namesProd=paste("y", 10:12, sep=""))

# Real simulation will need more than just 10 replications
Output <- simResult(10, Data.Mis, Model, objFunction=fun)
summary(Output)



cleanEx()
nameEx("simGamma")
### * simGamma

flush(stderr()); flush(stdout())

### Name: simGamma
### Title: Create random gamma distribution object
### Aliases: simGamma

### ** Examples

    g11 <- simGamma(1, 1)
    run(g11)
	summary(g11)



cleanEx()
nameEx("simGeom")
### * simGeom

flush(stderr()); flush(stdout())

### Name: simGeom
### Title: Create random geometric distribution object
### Aliases: simGeom

### ** Examples

    geom5 <- simGeom(0.05)
    run(geom5)
	summary(geom5)



cleanEx()
nameEx("simHyper")
### * simHyper

flush(stderr()); flush(stdout())

### Name: simHyper
### Title: Create random hypergeometric distribution object
### Aliases: simHyper

### ** Examples

    hyp <- simHyper(20, 5, 10)
    run(hyp)
	summary(hyp)



cleanEx()
nameEx("simLnorm")
### * simLnorm

flush(stderr()); flush(stdout())

### Name: simLnorm
### Title: Create random log normal distribution object
### Aliases: simLnorm

### ** Examples

    lognorm <- simLnorm(0, exp(1))
    run(lognorm)
	summary(lognorm)



cleanEx()
nameEx("simLogis")
### * simLogis

flush(stderr()); flush(stdout())

### Name: simLogis
### Title: Create random logistic distribution object
### Aliases: simLogis

### ** Examples

    logis <- simLogis(0, 1)
    run(logis)
	summary(logis)



cleanEx()
nameEx("simMatrix")
### * simMatrix

flush(stderr()); flush(stdout())

### Name: simMatrix
### Title: Create simMatrix that save free parameters and starting values,
###   as well as fixed values
### Aliases: simMatrix

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- simMatrix(loading, loadingValues)
summary(LX)
run(LX)

n65 <- simNorm(0.6, 0.05)
LY <- simMatrix(loading, "n65")
summary(LY)
run(LY)

start <- matrix(0, 6, 2)
start[1:3, 1] <- 0.7
start[4:6, 2] <- 0.7
ST <- simMatrix(value=start)



cleanEx()
nameEx("simMissing")
### * simMissing

flush(stderr()); flush(stdout())

### Name: simMissing
### Title: Construct a SimMissing object to create data with missingness
###   and analyze missing data.
### Aliases: simMissing

### ** Examples

	#Example of imposing 10% MCAR missing in all variables with no imputations (FIML method)
	Missing <- simMissing(pmMCAR=0.1)
	summary(Missing)
	
	loading <- matrix(0, 6, 1)
	loading[1:6, 1] <- NA
	LX <- simMatrix(loading, 0.7)
	RPH <- symMatrix(diag(1))
	RTD <- symMatrix(diag(6))
	CFA.Model <- simSetCFA(LY = LX, RPS = RPH, RTE = RTD)
	SimData <- simData(CFA.Model, 500)
	SimModel <- simModel(CFA.Model)
	
	#Create data
	dat <- run(SimData)
	
	#Impose missing
	dat <- run(Missing, dat)
	
	#Analyze data
	out <- run(SimModel, dat)
	summary(out)
	
	#Example to create simMissing object for 3 forms design at 3 timepoints with 10 imputations
	Missing <- simMissing(nforms=3, timePoints=3, numImps=10)




cleanEx()
nameEx("simMisspecCFA")
### * simMisspecCFA

flush(stderr()); flush(stdout())

### Name: simMisspecCFA
### Title: Set of model misspecification for CFA model.
### Aliases: simMisspecCFA

### ** Examples

n01 <- simNorm(0, 0.1)
error.cor.Mis <- matrix(NA, 6, 6)
diag(error.cor.Mis) <- 1
RTD.Mis <- symMatrix(error.cor.Mis, "n01")
CFA.Model.Mis <- simMisspecCFA(RTD=RTD.Mis)



cleanEx()
nameEx("simMisspecPath")
### * simMisspecPath

flush(stderr()); flush(stdout())

### Name: simMisspecPath
### Title: Set of model misspecification for Path analysis model.
### Aliases: simMisspecPath

### ** Examples

u1 <- simUnif(-0.1, 0.1)
mis.path.GA <- matrix(0, 2, 2)
mis.path.GA[2, 1:2] <- NA
mis.GA <- simMatrix(mis.path.GA, "u1")
Path.Mis.Model <- simMisspecPath(GA = mis.GA, exo=TRUE)



cleanEx()
nameEx("simMisspecSEM")
### * simMisspecSEM

flush(stderr()); flush(stdout())

### Name: simMisspecSEM
### Title: Set of model misspecification for SEM model.
### Aliases: simMisspecSEM

### ** Examples

u2 <- simUnif(-0.2, 0.2)
n1 <- simNorm(0, 0.1)
loading.X.trivial <- matrix(NA, 6, 2)
loading.X.trivial[is.na(loading.X.trivial)] <- 0
LX.trivial <- simMatrix(loading.X.trivial, "u2")
error.cor.X.trivial <- matrix(NA, 6, 6)
diag(error.cor.X.trivial) <- 0
RTD.trivial <- symMatrix(error.cor.X.trivial, "n1")
error.cor.Y.trivial <- matrix(NA, 2, 2)
diag(error.cor.Y.trivial) <- 0
RTE.trivial <- symMatrix(error.cor.Y.trivial, "n1")
RTH.trivial <- simMatrix(matrix(NA, 6, 2), "n1")
SEM.Mis.Model <- simMisspecSEM(LX = LX.trivial, RTE = RTE.trivial, RTD = RTD.trivial, RTH = RTH.trivial, exo=TRUE)



cleanEx()
nameEx("simModel")
### * simModel

flush(stderr()); flush(stdout())

### Name: simModel
### Title: Create simModel from model specification and be ready for data
###   analysis.
### Aliases: simModel simModel-methods simModel,ANY-method
###   simModel,SimSet-method simModel,SimParam-method
###   simModel,SimModelOut-method

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- simMatrix(loading, loadingValues)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, 0.5)
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
RTD <- symMatrix(error.cor)
CFA.Model <- simSetCFA(LX = LX, RPH = RPH, RTD = RTD)
SimModel <- simModel(CFA.Model)

library(lavaan)
loading <- matrix(0, 9, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
HS.Model <- simParamCFA(LX = loading)
SimModel <- simModel(HS.Model, indLab=paste("x", 1:9, sep=""))
out <- run(SimModel, HolzingerSwineford1939)
summary(out)



cleanEx()
nameEx("simNbinom")
### * simNbinom

flush(stderr()); flush(stdout())

### Name: simNbinom
### Title: Create random negative binomial distribution object
### Aliases: simNbinom

### ** Examples

    nbinom <- simNbinom(5, 0.25)
    run(nbinom)
	summary(nbinom)



cleanEx()
nameEx("simNorm")
### * simNorm

flush(stderr()); flush(stdout())

### Name: simNorm
### Title: Create random normal distribution object
### Aliases: simNorm

### ** Examples

    n02 <- simNorm(0, 0.2)
    run(n02)
	summary(n02)



cleanEx()
nameEx("simParamCFA")
### * simParamCFA

flush(stderr()); flush(stdout())

### Name: simParamCFA
### Title: Create a set of matrices of parameters for analyzing data that
###   belongs to CFA model.
### Aliases: simParamCFA

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
CFA.Model <- simParamCFA(LX = loading)



cleanEx()
nameEx("simParamPath")
### * simParamPath

flush(stderr()); flush(stdout())

### Name: simParamPath
### Title: Create a set of matrices of parameters for analyzing data that
###   belongs to Path analysis model
### Aliases: simParamPath

### ** Examples
 
path <- matrix(0, 4, 4)
path[3, 1:2] <- NA
path[4, 3] <- NA
model <- simParamPath(BE=path)

exoPath <- matrix(NA, 3, 2)
model2 <- simParamPath(GA=exoPath, exo=TRUE)



cleanEx()
nameEx("simParamSEM")
### * simParamSEM

flush(stderr()); flush(stdout())

### Name: simParamSEM
### Title: Create a set of matrices of parameters for analyzing data that
###   belongs to SEM model
### Aliases: simParamSEM

### ** Examples

loading <- matrix(0, 8, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:8, 3] <- NA
path <- matrix(0, 3, 3)
path[3, 1:2] <- NA
SEM.model <- simParamSEM(BE=path, LY=loading)

loading.X <- matrix(0, 6, 2)
loading.X[1:3, 1] <- NA
loading.X[4:6, 2] <- NA
loading.Y <- matrix(NA, 2, 1)
path.GA <- matrix(NA, 1, 2)
BE <- as.matrix(0)
SEM.Exo.model <- simParamSEM(GA=path.GA, BE=BE, LX=loading.X, LY=loading.Y, exo=TRUE)



cleanEx()
nameEx("simPois")
### * simPois

flush(stderr()); flush(stdout())

### Name: simPois
### Title: Create random Poisson distribution object
### Aliases: simPois

### ** Examples

    pois5 <- simPois(5)
    run(pois5)
	summary(pois5)



cleanEx()
nameEx("simResult")
### * simResult

flush(stderr()); flush(stdout())

### Name: simResult
### Title: Create simResult.
### Aliases: simResult

### ** Examples

loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- simMatrix(loading, 0.7)
RPH <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model <- simSetCFA(LY = LX, RPS = RPH, RTE = RTD)
SimData <- simData(CFA.Model, 500)
SimModel <- simModel(CFA.Model)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
Output <- simResult(50, SimData, SimModel)
summary(Output)

# Specify Sample Size by n
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- simMatrix(loading, 0.7)
RPH <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model <- simSetCFA(LY = LX, RPS = RPH, RTE = RTD)
SimData <- simData(CFA.Model, 500)
SimModel <- simModel(CFA.Model)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
Output <- simResult(NULL, SimData, SimModel, n=seq(50, 500, 10))
summary(Output)

# Specify both sample size and percent missing completely at random
Output <- simResult(NULL, SimData, SimModel, n=seq(50, 500, 25), pmMCAR=c(0, 0.1, 0.2))
summary(Output)

# Use distribution object on sample size and percent completely at random
n <- simUnif(100, 500)
pmMCAR <- simUnif(0, 0.1)
Output <- simResult(50, SimData, SimModel, n=n, pmMCAR=pmMCAR)




cleanEx()
nameEx("simResultParam")
### * simResultParam

flush(stderr()); flush(stdout())

### Name: simResultParam
### Title: The constructor of the parameter result object
### Aliases: simResultParam

### ** Examples

u35 <- simUnif(0.3, 0.5)
u57 <- simUnif(0.5, 0.7)
u1 <- simUnif(-0.1, 0.1)
n31 <- simNorm(0.3, 0.1)

path.BE <- matrix(0, 4, 4)
path.BE[3, 1:2] <- NA
path.BE[4, 3] <- NA
starting.BE <- matrix("", 4, 4)
starting.BE[3, 1:2] <- "u35"
starting.BE[4, 3] <- "u57"
BE <- simMatrix(path.BE, starting.BE)

residual.error <- diag(4)
residual.error[1,2] <- residual.error[2,1] <- NA
RPS <- symMatrix(residual.error, "n31")

ME <- simVector(rep(NA, 4), 0)

Path.Model <- simSetPath(RPS = RPS, BE = BE, ME = ME)

mis.path.BE <- matrix(0, 4, 4)
mis.path.BE[4, 1:2] <- NA
mis.BE <- simMatrix(mis.path.BE, "u1")
Path.Mis.Model <- simMisspecPath(BE = mis.BE, misfitType="rmsea")

# The number of replications in actual analysis should be much more than 50
ParamObject <- simResultParam(50, Path.Model, Path.Mis.Model)

# Specify the range of misfits to select the set of misspecified parameters
Path.Mis.Model2 <- simMisspecPath(BE = mis.BE, misfitType="rmsea", misfitBound=c(0.05, 0.08))
ParamObject2 <- simResultParam(50, Path.Model, Path.Mis.Model2)

# Find the maximum misspecification for each actual parameter
Path.Mis.Model3 <- simMisspecPath(BE = mis.BE, misfitType="rmsea", optMisfit="max", numIter=10)
ParamObject3 <- simResultParam(50, Path.Model, Path.Mis.Model3)



cleanEx()
nameEx("simSetCFA")
### * simSetCFA

flush(stderr()); flush(stdout())

### Name: simSetCFA
### Title: Create a set of matrices of parameter and parameter values to
###   generate and analyze data that belongs to CFA model.
### Aliases: simSetCFA

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- simMatrix(loading, loadingValues)
summary(LX)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, 0.5)

error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
RTD <- symMatrix(error.cor)

CFA.Model <- simSetCFA(LX = LX, RPH = RPH, RTD = RTD)



cleanEx()
nameEx("simSetPath")
### * simSetPath

flush(stderr()); flush(stdout())

### Name: simSetPath
### Title: Create a set of matrices of parameter and parameter values to
###   generate and analyze data that belongs to Path analysis model
### Aliases: simSetPath

### ** Examples
 
u35 <- simUnif(0.3, 0.5)
u57 <- simUnif(0.5, 0.7)
u1 <- simUnif(-0.1, 0.1)
n31 <- simNorm(0.3, 0.1)

path.BE <- matrix(0, 4, 4)
path.BE[3, 1:2] <- NA
path.BE[4, 3] <- NA
starting.BE <- matrix("", 4, 4)
starting.BE[3, 1:2] <- "u35"
starting.BE[4, 3] <- "u57"
BE <- simMatrix(path.BE, starting.BE)

residual.error <- diag(4)
residual.error[1,2] <- residual.error[2,1] <- NA
RPS <- symMatrix(residual.error, "n31")

Path.Model <- simSetPath(RPS = RPS, BE = BE)

u35 <- simUnif(0.3, 0.5)
u57 <- simUnif(0.5, 0.7)
u1 <- simUnif(-0.1, 0.1)
n31 <- simNorm(0.3, 0.1)

path.GA <- matrix(0, 2, 2)
path.GA[1, 1:2] <- NA
GA <- simMatrix(path.GA, "u35")

path.BE <- matrix(0, 2, 2)
path.BE[2, 1] <- NA
BE <- simMatrix(path.BE, "u57")

exo.cor <- matrix(NA, 2, 2)
diag(exo.cor) <- 1
RPH <- symMatrix(exo.cor, "n31")

RPS <- symMatrix(diag(2))

Path.Exo.Model <- simSetPath(RPS = RPS, BE = BE, RPH = RPH, GA = GA, exo=TRUE)



cleanEx()
nameEx("simSetSEM")
### * simSetSEM

flush(stderr()); flush(stdout())

### Name: simSetSEM
### Title: Create a set of matrices of parameter and parameter values to
###   generate and analyze data that belongs to SEM model
### Aliases: simSetSEM

### ** Examples

u35 <- simUnif(0.3, 0.5)
u68 <- simUnif(0.6, 0.8)
n65 <- simNorm(0.6, 0.05)
loading <- matrix(0, 8, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:8, 3] <- NA
loading.start <- matrix("", 8, 3)
loading.start[1:3, 1] <- 0.7
loading.start[4:6, 2] <- 0.7
loading.start[7:8, 3] <- "u68"
LY <- simMatrix(loading, loading.start)

RTE <- symMatrix(diag(8))

factor.cor <- diag(3)
factor.cor[1, 2] <- factor.cor[2, 1] <- NA
RPS <- symMatrix(factor.cor, 0.5)

path <- matrix(0, 3, 3)
path[3, 1:2] <- NA
path.start <- matrix(0, 3, 3)
path.start[3, 1] <- "n65"
path.start[3, 2] <- "u35"
BE <- simMatrix(path, path.start)

SEM.model <- simSetSEM(BE=BE, LY=LY, RPS=RPS, RTE=RTE)

loading.X <- matrix(0, 6, 2)
loading.X[1:3, 1] <- NA
loading.X[4:6, 2] <- NA
LX <- simMatrix(loading.X, 0.7)

loading.Y <- matrix(NA, 2, 1)
LY <- simMatrix(loading.Y, "u68")

RTD <- symMatrix(diag(6))

RTE <- symMatrix(diag(2))

factor.K.cor <- matrix(NA, 2, 2)
diag(factor.K.cor) <- 1
RPH <- symMatrix(factor.K.cor, 0.5)

RPS <- symMatrix(as.matrix(1))

path.GA <- matrix(NA, 1, 2)
path.GA.start <- matrix(c("n65", "u35"), ncol=2)
GA <- simMatrix(path.GA, path.GA.start)

BE <- simMatrix(as.matrix(0))

SEM.Exo.model <- simSetSEM(GA=GA, BE=BE, LX=LX, LY=LY, RPH=RPH, RPS=RPS, RTD=RTD, RTE=RTE, exo=TRUE)



cleanEx()
nameEx("simT")
### * simT

flush(stderr()); flush(stdout())

### Name: simT
### Title: Create random t distribution object
### Aliases: simT

### ** Examples

    nct82 <- simT(8, ncp=2)
    run(nct82)
	summary(nct82)



cleanEx()
nameEx("simUnif")
### * simUnif

flush(stderr()); flush(stdout())

### Name: simUnif
### Title: Create random uniform distribution object
### Aliases: simUnif

### ** Examples

u1 <- simUnif(-0.1, 0.1)
run(u1)
summary(u1)



cleanEx()
nameEx("simVector")
### * simVector

flush(stderr()); flush(stdout())

### Name: simVector
### Title: Create simVector that save free parameters and starting values,
###   as well as fixed values
### Aliases: simVector

### ** Examples

factor.mean <- rep(NA, 4)
AL <- simVector(factor.mean, 0)

n02 <- simNorm(0, 0.2)
factor.start <- rep("n02", 4)
KA <- simVector(factor.mean, factor.start)

start <- c(2, 0, 0, 1)
VE <- simVector(value=start)



cleanEx()
nameEx("simWeibull")
### * simWeibull

flush(stderr()); flush(stdout())

### Name: simWeibull
### Title: Create random Weibull distribution object
### Aliases: simWeibull

### ** Examples

    exWeibull <- simWeibull(2, 100)
    run(exWeibull)
	summary(exWeibull)



cleanEx()
nameEx("skew")
### * skew

flush(stderr()); flush(stdout())

### Name: skew
### Title: Finding skewness
### Aliases: skew skew-methods skew,vector-method

### ** Examples

skew(1:5)



cleanEx()
nameEx("standardize")
### * standardize

flush(stderr()); flush(stdout())

### Name: standardize
### Title: Standardize the parameter estimates within an object
### Aliases: standardize standardize-methods standardize,ANY-method
###   standardize,SimModelOut-method standardize,SimRSet-method

### ** Examples

# This function is not public.

# loading <- matrix(0, 6, 2)
# loading[1:3, 1] <- NA
# loading[4:6, 2] <- NA
# loadingValues <- matrix(0, 6, 2)
# loadingValues[1:3, 1] <- 0.7
# loadingValues[4:6, 2] <- 0.7
# LX <- simMatrix(loading, loadingValues)
# summary(LX)
# latent.cor <- matrix(NA, 2, 2)
# diag(latent.cor) <- 1
# PH <- symMatrix(latent.cor, 0.5)
# error.cor <- matrix(0, 6, 6)
# diag(error.cor) <- 1
# TD <- symMatrix(error.cor)
# CFA.Model <- simSetCFA(LX = LX, PH = PH, TD = TD)
# SimData <- simData(CFA.Model, 200)
# SimModel <- simModel(CFA.Model)
# standardize(run(SimModel, run(SimData)))

# loading <- matrix(0, 6, 2)
# loading[1:3, 1] <- NA
# loading[4:6, 2] <- NA
# loadingValues <- matrix(0, 6, 2)
# loadingValues[1:3, 1] <- 0.7
# loadingValues[4:6, 2] <- 0.7
# LX <- simMatrix(loading, loadingValues)
# summary(LX)
# latent.cor <- matrix(NA, 2, 2)
# diag(latent.cor) <- 1
# PH <- symMatrix(latent.cor, 0.5)
# error.cor <- matrix(0, 6, 6)
# diag(error.cor) <- 1
# TD <- symMatrix(error.cor)
# CFA.Model <- simSetCFA(LX = LX, PH = PH, TD = TD)
# set <- reduceMatrices(run(CFA.Model))



cleanEx()
nameEx("startingValues")
### * startingValues

flush(stderr()); flush(stdout())

### Name: startingValues
### Title: Find starting values by averaging random numbers
### Aliases: startingValues startingValues-methods
###   startingValues,ANY-method startingValues,SimMatrix-method
###   startingValues,SimVector-method startingValues,SimSet-method

### ** Examples

# This function is not public

#u89 <- simUnif(0.8, 0.9)
#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#LX <- simMatrix(loading, "u89")
#startingValues(LX, 10)

#u89 <- simUnif(0.8, 0.9)
#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#LX <- simMatrix(loading, "u89")
#latent.cor <- matrix(NA, 2, 2)
#diag(latent.cor) <- 1
#PH <- symMatrix(latent.cor, 0.5)
#error.cor <- matrix(0, 6, 6)
#diag(error.cor) <- 1
#TD <- symMatrix(error.cor)
#CFA.Model <- simSetCFA(LX = LX, PH = PH, TD = TD)
#result <- startingValues(CFA.Model, 10)
#summary(result)



cleanEx()
nameEx("subtractObject")
### * subtractObject

flush(stderr()); flush(stdout())

### Name: subtractObject
### Title: Make a subtraction of each element in an object
### Aliases: subtractObject subtractObject-methods
###   subtractObject,ANY,ANY-method subtractObject,SimRSet,SimRSet-method

### ** Examples

# This function is not public

#u89 <- simUnif(0.8, 0.9)
#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#LX <- simMatrix(loading, "u89")
#startingValues(LX, 10)

#u89 <- simUnif(0.8, 0.9)
#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#LX <- simMatrix(loading, "u89")
#latent.cor <- matrix(NA, 2, 2)
#diag(latent.cor) <- 1
#PH <- symMatrix(latent.cor, 0.5)
#error.cor <- matrix(0, 6, 6)
#diag(error.cor) <- 1
#TD <- symMatrix(error.cor)
#CFA.Model <- simSetCFA(LX = LX, PH = PH, TD = TD)
#result <- startingValues(CFA.Model, 10)
#summary(result)



cleanEx()
nameEx("summaryParam")
### * summaryParam

flush(stderr()); flush(stdout())

### Name: summaryParam
### Title: Provide summary of parameter estimates and standard error across
###   replications
### Aliases: summaryParam summaryParam-methods summaryParam,ANY-method
###   summaryParam,SimResult-method summaryParam,SimModelOut-method
###   summaryParam,SimModelMIOut-method

### ** Examples

showClass("SimResult")
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- simMatrix(loading, 0.7)
RPH <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model <- simSetCFA(LY = LX, RPS = RPH, RTE = RTD)
SimData <- simData(CFA.Model, 500)
SimModel <- simModel(CFA.Model)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
Output <- simResult(50, SimData, SimModel)
summaryParam(Output)
summaryParam(Output, detail=TRUE)



cleanEx()
nameEx("summaryPopulation")
### * summaryPopulation

flush(stderr()); flush(stdout())

### Name: summaryPopulation
### Title: Summarize the data generation population model underlying an
###   object
### Aliases: summaryPopulation summaryPopulation-methods
###   summaryPopulation,ANY-method

### ** Examples

# See each class for an example.



cleanEx()
nameEx("summaryShort")
### * summaryShort

flush(stderr()); flush(stdout())

### Name: summaryShort
### Title: Provide short summary of an object.
### Aliases: summaryShort summaryShort-methods summaryShort,ANY-method
###   summaryShort,vector-method summaryShort,matrix-method

### ** Examples

u89 <- simUnif(0.8, 0.9)
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
LX <- simMatrix(loading, "u89")
summaryShort(LX)



cleanEx()
nameEx("symMatrix")
### * symMatrix

flush(stderr()); flush(stdout())

### Name: symMatrix
### Title: Create symmetric simMatrix that save free parameters and
###   starting values, as well as fixed values
### Aliases: symMatrix

### ** Examples

latent.cor <- matrix(NA, 3, 3)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, 0.5)

u46 <- simUnif(0.4, 0.6)
factor.cor <- matrix(NA, 4, 4)
diag(factor.cor) <- 1
factor.cor.start <- matrix("u46", 4, 4)
factor.cor.start[1, 2] <- factor.cor.start[2, 1] <- "0.5"
RPS <- symMatrix(factor.cor, factor.cor.start)

start <- diag(4)
start[1, 2] <- 0.5
start[2, 1] <- 0.5
ST <- symMatrix(value=start)



cleanEx()
nameEx("tagHeaders")
### * tagHeaders

flush(stderr()); flush(stdout())

### Name: tagHeaders
### Title: Tag names to each element
### Aliases: tagHeaders tagHeaders-methods tagHeaders,ANY-method
###   tagHeaders,VirtualRSet-method

### ** Examples

# No example



cleanEx()
nameEx("toFunction")
### * toFunction

flush(stderr()); flush(stdout())

### Name: toFunction
### Title: Export the distribution object to a function command in text
###   that can be evaluated directly.
### Aliases: toFunction toFunction-methods toFunction,ANY-method

### ** Examples

u2 <- simUnif(-0.2, 0.2)
toFunction(u2)



cleanEx()
nameEx("toSimSet")
### * toSimSet

flush(stderr()); flush(stdout())

### Name: toSimSet
### Title: Transform the analysis model object into the object for data
###   generation
### Aliases: toSimSet toSimSet-methods toSimSet,ANY-method
###   toSimSet,SimRSet-method toSimSet,SimModelOut-method

### ** Examples

# This function is not public.

# library(lavaan)
# hs <- HolzingerSwineford1939
# loading <- matrix(0, 9, 3)
# loading[1:3, 1] <- NA
# loading[4:6, 2] <- NA
# loading[7:9, 3] <- NA
# model <- simParamCFA(LY=loading)
# SimModel <- simModel(model, indLab=paste("x", 1:9, sep=""))
# out <- run(SimModel, hs)
# set <- toSimSet(out)



cleanEx()
nameEx("validateCovariance")
### * validateCovariance

flush(stderr()); flush(stdout())

### Name: validateCovariance
### Title: Validate whether all elements provides a good covariance matrix
### Aliases: validateCovariance

### ** Examples

# No example



cleanEx()
nameEx("validateObject")
### * validateObject

flush(stderr()); flush(stdout())

### Name: validateObject
### Title: Validate whether the drawn parameters are good.
### Aliases: validateObject

### ** Examples

# No example



cleanEx()
nameEx("validatePath")
### * validatePath

flush(stderr()); flush(stdout())

### Name: validatePath
### Title: Validate whether the regression coefficient (or loading) matrix
###   is good
### Aliases: validatePath

### ** Examples

# No example



cleanEx()
nameEx("vectorizeObject")
### * vectorizeObject

flush(stderr()); flush(stdout())

### Name: vectorizeObject
### Title: Change an object to a vector with labels
### Aliases: vectorizeObject vectorizeObject-methods
###   vectorizeObject,ANY,ANY-method vectorizeObject,vector,vector-method
###   vectorizeObject,matrix,matrix-method
###   vectorizeObject,VirtualRSet,SimLabels-method
###   vectorizeObject,MatrixSet,SimGenLabels-method

### ** Examples

# No example



cleanEx()
nameEx("weightedMean")
### * weightedMean

flush(stderr()); flush(stdout())

### Name: weightedMean
### Title: Calculate the weighted mean of a variable
### Aliases: weightedMean

### ** Examples

# This function is not public

# weightedMean(1:5, c(1,1,1,1,2))



cleanEx()
nameEx("writeLavaanCode")
### * writeLavaanCode

flush(stderr()); flush(stdout())

### Name: writeLavaanCode
### Title: Write a lavaan code given the matrices of free parameter
### Aliases: writeLavaanCode

### ** Examples

# No example



cleanEx()
nameEx("writeLavaanConstraint")
### * writeLavaanConstraint

flush(stderr()); flush(stdout())

### Name: writeLavaanConstraint
### Title: Write a lavaan code for a given set of equality constraints
### Aliases: writeLavaanConstraint

### ** Examples

# No example



cleanEx()
nameEx("writeLavaanIndividualConstraint")
### * writeLavaanIndividualConstraint

flush(stderr()); flush(stdout())

### Name: writeLavaanIndividualConstraint
### Title: Write a lavaan code for a given equality constraint for each
###   parameter
### Aliases: writeLavaanIndividualConstraint

### ** Examples

# No example



cleanEx()
nameEx("writeLavaanNullCode")
### * writeLavaanNullCode

flush(stderr()); flush(stdout())

### Name: writeLavaanNullCode
### Title: Write a lavaan code for a null model
### Aliases: writeLavaanNullCode

### ** Examples

# No example



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
