######### This file save the example that specifies dontrun in it. 

#################### continuousPower

loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- simMatrix(loading, 0.7)
RPH <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model <- simSetCFA(LY = LX, RPS = RPH, RTE = RTD)
SimData <- simData(CFA.Model, 500)
SimModel <- simModel(CFA.Model)
# We will use only 5 replications to save time.
# In reality, more replications are needed.

# Specify both sample size and percent missing completely at random
Output <- simResult(NULL, SimData, SimModel, n=seq(100, 200, 20), pmMCAR=c(0, 0.1, 0.2))
summary(Output)

Cpow <- continuousPower(Output, contN = TRUE, contMCAR = TRUE)
Cpow

Cpow2 <- continuousPower(Output, contN = TRUE, contMCAR = TRUE, pred=list(N = 200, pmMCAR = 0.3))
Cpow2

#################### findPower

# Specify Sample Size by n
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- simMatrix(loading, 0.4)
RPH <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model <- simSetCFA(LY = LX, RPS = RPH, RTE = RTD)
SimData <- simData(CFA.Model, 500)
SimModel <- simModel(CFA.Model)

# Specify both sample size and percent missing completely at random
Output <- simResult(NULL, SimData, SimModel, n=seq(100, 200, 20), pmMCAR=c(0, 0.1, 0.2))
pow <- getPower(Output)
findPower(pow, "N", 0.80)

#################### getCutoff

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
# We make the examples running only 5 replications to save time.
# In reality, more replications are needed.
Output <- simResult(5, SimData, SimModel)
getCutoff(Output, 0.05)

# Finding the cutoff when the sample size is varied.
Output2 <- simResult(NULL, SimData, SimModel, n=seq(50, 100, 10))
getCutoff(Output2, 0.05, nVal = 75)

#################### getCutoffNested

n1 <- simNorm(0, 0.1)
u79 <- simUnif(0.7, 0.9)

loading.null <- matrix(0, 6, 1)
loading.null[1:6, 1] <- NA
LX.NULL <- simMatrix(loading.null, 0.7)
RPH.NULL <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model.NULL <- simSetCFA(LY = LX.NULL, RPS = RPH.NULL, RTE = RTD)

error.cor.mis <- matrix(NA, 6, 6)
diag(error.cor.mis) <- 1
RTD.Mis <- symMatrix(error.cor.mis, "n1")
CFA.Model.NULL.Mis <- simMisspecCFA(RTE = RTD.Mis)

loading.alt <- matrix(0, 6, 2)
loading.alt[1:3, 1] <- NA
loading.alt[4:6, 2] <- NA
LX.ALT <- simMatrix(loading.alt, 0.7)
latent.cor.alt <- matrix(NA, 2, 2)
diag(latent.cor.alt) <- 1
RPH.ALT <- symMatrix(latent.cor.alt, "u79")
CFA.Model.ALT <- simSetCFA(LY = LX.ALT, RPS = RPH.ALT, RTE = RTD)

SimData.NULL <- simData(CFA.Model.NULL, 500)

SimModel.NULL <- simModel(CFA.Model.NULL)
SimModel.ALT <- simModel(CFA.Model.ALT)

# The actual number of replications should be greater than 10.
Output.NULL.NULL <- simResult(10, SimData.NULL, SimModel.NULL)
Output.NULL.ALT <- simResult(10, SimData.NULL, SimModel.ALT)

getCutoffNested(Output.NULL.NULL, Output.NULL.ALT)

#################### getCutoffNonNested

n1 <- simNorm(0, 0.1)
u79 <- simUnif(0.7, 0.9)

loading.A <- matrix(0, 8, 2)
loading.A[1:3, 1] <- NA
loading.A[4:8, 2] <- NA
LX.A <- simMatrix(loading.A, 0.7)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, "u79")
RTD <- symMatrix(diag(8))
CFA.Model.A <- simSetCFA(LY = LX.A, RPS = RPH, RTE = RTD)

error.cor.mis <- matrix(NA, 8, 8)
diag(error.cor.mis) <- 1
RTD.Mis <- symMatrix(error.cor.mis, "n1")
CFA.Model.A.Mis <- simMisspecCFA(RTE = RTD.Mis)

loading.B <- matrix(0, 8, 2)
loading.B[1:4, 1] <- NA
loading.B[5:8, 2] <- NA
LX.B <- simMatrix(loading.B, 0.7)
CFA.Model.B <- simSetCFA(LY = LX.B, RPS = RPH, RTE = RTD)

SimData.A <- simData(CFA.Model.A, 500)
SimData.B <- simData(CFA.Model.B, 500)

SimModel.A <- simModel(CFA.Model.A)
SimModel.B <- simModel(CFA.Model.B)

# The actual number of replications should be greater than 10.
Output.A.A <- simResult(10, SimData.A, SimModel.A)
Output.A.B <- simResult(10, SimData.A, SimModel.B)
Output.B.A <- simResult(10, SimData.B, SimModel.A)
Output.B.B <- simResult(10, SimData.B, SimModel.B)

getCutoffNonNested(Output.A.A, Output.A.B, Output.B.A, Output.B.B)
getCutoffNonNested(Output.A.A, Output.A.B)
getCutoffNonNested(Output.B.B, Output.B.A)

#################### getPower

loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- simMatrix(loading, 0.7)
RPH <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model <- simSetCFA(LY = LX, RPS = RPH, RTE = RTD)
SimData <- simData(CFA.Model, 500)
SimModel <- simModel(CFA.Model)
# We will use only 5 replications to save time.
# In reality, more replications are needed.

# Specify both sample size and percent missing completely at random
Output <- simResult(NULL, SimData, SimModel, n=seq(100, 200, 20), pmMCAR=c(0, 0.1, 0.2))
summary(Output)

getPower(Output)

getPower(Output, nVal=c(100, 200), pmMCARval=c(0, 0.1, 0.2))

#################### getPowerFit

u2 <- simUnif(-0.2, 0.2)
n1 <- simNorm(0, 0.1)
u79 <- simUnif(0.7, 0.9)

loading.null <- matrix(0, 6, 1)
loading.null[1:6, 1] <- NA
LX.NULL <- simMatrix(loading.null, 0.7)
RPH.NULL <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model.NULL <- simSetCFA(LY = LX.NULL, RPS = RPH.NULL, RTE = RTD)

error.cor.mis <- matrix(NA, 6, 6)
diag(error.cor.mis) <- 1
RTD.Mis <- symMatrix(error.cor.mis, "rnorm(1,0,0.1)")
CFA.Model.NULL.Mis <- simMisspecCFA(RTE = RTD.Mis)

loading.alt <- matrix(0, 6, 2)
loading.alt[1:3, 1] <- NA
loading.alt[4:6, 2] <- NA
LX.ALT <- simMatrix(loading.alt, 0.7)
latent.cor.alt <- matrix(NA, 2, 2)
diag(latent.cor.alt) <- 1
RPH.ALT <- symMatrix(latent.cor.alt, 0.7)
CFA.Model.ALT <- simSetCFA(LY = LX.ALT, RPS = RPH.ALT, RTE = RTD)

# loading.alt.mis <- matrix(NA, 6, 2)
# loading.alt.mis[is.na(loading.alt)] <- 0
# LX.alt.mis <- simMatrix(loading.alt.mis, "runif(1,-.2,.2)")
# CFA.Model.alt.mis <- simMisspecCFA(LY = LX.alt.mis, RTE=RTD.Mis)

SimData.NULL <- simData(CFA.Model.NULL, 500)
SimData.ALT <- simData(CFA.Model.ALT, 500)

SimModel.NULL <- simModel(CFA.Model.NULL)
SimModel.ALT <- simModel(CFA.Model.ALT)

Output.NULL.NULL <- simResult(10, SimData.NULL, SimModel.NULL)
Output.ALT.NULL <- simResult(10, SimData.ALT, SimModel.NULL)
Output.NULL.ALT <- simResult(10, SimData.NULL, SimModel.ALT)
Output.ALT.ALT <- simResult(10, SimData.ALT, SimModel.ALT)

getPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, nullNested=Output.NULL.NULL, nullParent=Output.NULL.ALT)
getPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, cutoff=c(Chi=3.84, CFI=-0.10))

Output.NULL.NULL2 <- simResult(NULL, SimData.NULL, SimModel.NULL, n=seq(50, 500, 50))
Output.ALT.NULL2 <- simResult(NULL, SimData.ALT, SimModel.NULL, n=seq(50, 500, 50))
Output.NULL.ALT2 <- simResult(NULL, SimData.NULL, SimModel.ALT, n=seq(50, 500, 50))
Output.ALT.ALT2 <- simResult(NULL, SimData.ALT, SimModel.ALT, n=seq(50, 500, 50))

getPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, nullNested=Output.NULL.NULL, nullParent=Output.NULL.ALT, nVal = 250)
getPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, cutoff=c(Chi=3.84, CFI=-0.10), nVal = 250)

#################### getPowerFitNested

u2 <- simUnif(-0.2, 0.2)
n1 <- simNorm(0, 0.1)
u79 <- simUnif(0.7, 0.9)

loading.null <- matrix(0, 6, 1)
loading.null[1:6, 1] <- NA
LX.NULL <- simMatrix(loading.null, 0.7)
RPH.NULL <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model.NULL <- simSetCFA(LY = LX.NULL, RPS = RPH.NULL, RTE = RTD)

error.cor.mis <- matrix(NA, 6, 6)
diag(error.cor.mis) <- 1
RTD.Mis <- symMatrix(error.cor.mis, "rnorm(1,0,0.1)")
CFA.Model.NULL.Mis <- simMisspecCFA(RTE = RTD.Mis)

loading.alt <- matrix(0, 6, 2)
loading.alt[1:3, 1] <- NA
loading.alt[4:6, 2] <- NA
LX.ALT <- simMatrix(loading.alt, 0.7)
latent.cor.alt <- matrix(NA, 2, 2)
diag(latent.cor.alt) <- 1
RPH.ALT <- symMatrix(latent.cor.alt, 0.7)
CFA.Model.ALT <- simSetCFA(LY = LX.ALT, RPS = RPH.ALT, RTE = RTD)

# loading.alt.mis <- matrix(NA, 6, 2)
# loading.alt.mis[is.na(loading.alt)] <- 0
# LX.alt.mis <- simMatrix(loading.alt.mis, "runif(1,-.2,.2)")
# CFA.Model.alt.mis <- simMisspecCFA(LY = LX.alt.mis, RTE=RTD.Mis)

SimData.NULL <- simData(CFA.Model.NULL, 500)
SimData.ALT <- simData(CFA.Model.ALT, 500)

SimModel.NULL <- simModel(CFA.Model.NULL)
SimModel.ALT <- simModel(CFA.Model.ALT)

Output.NULL.NULL <- simResult(10, SimData.NULL, SimModel.NULL)
Output.ALT.NULL <- simResult(10, SimData.ALT, SimModel.NULL)
Output.NULL.ALT <- simResult(10, SimData.NULL, SimModel.ALT)
Output.ALT.ALT <- simResult(10, SimData.ALT, SimModel.ALT)

getPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, nullNested=Output.NULL.NULL, nullParent=Output.NULL.ALT)
getPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, cutoff=c(Chi=3.84, CFI=-0.10))

Output.NULL.NULL2 <- simResult(NULL, SimData.NULL, SimModel.NULL, n=seq(50, 500, 50))
Output.ALT.NULL2 <- simResult(NULL, SimData.ALT, SimModel.NULL, n=seq(50, 500, 50))
Output.NULL.ALT2 <- simResult(NULL, SimData.NULL, SimModel.ALT, n=seq(50, 500, 50))
Output.ALT.ALT2 <- simResult(NULL, SimData.ALT, SimModel.ALT, n=seq(50, 500, 50))

getPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, nullNested=Output.NULL.NULL, nullParent=Output.NULL.ALT, nVal = 250)
getPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, cutoff=c(Chi=3.84, CFI=-0.10), nVal = 250)

#################### getPowerFitNonNested

n1 <- simNorm(0, 0.1)
u79 <- simUnif(0.7, 0.9)

loading.A <- matrix(0, 8, 2)
loading.A[1:3, 1] <- NA
loading.A[4:8, 2] <- NA
LX.A <- simMatrix(loading.A, 0.7)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, "u79")
RTD <- symMatrix(diag(8))
CFA.Model.A <- simSetCFA(LY = LX.A, RPS = RPH, RTE = RTD)

error.cor.mis <- matrix(NA, 8, 8)
diag(error.cor.mis) <- 1
RTD.Mis <- symMatrix(error.cor.mis, "n1")
CFA.Model.A.Mis <- simMisspecCFA(RTE = RTD.Mis)

loading.B <- matrix(0, 8, 2)
loading.B[1:4, 1] <- NA
loading.B[5:8, 2] <- NA
LX.B <- simMatrix(loading.B, 0.7)
CFA.Model.B <- simSetCFA(LY = LX.B, RPS = RPH, RTE = RTD)

SimData.A <- simData(CFA.Model.A, 500)
SimData.B <- simData(CFA.Model.B, 500)

SimModel.A <- simModel(CFA.Model.A)
SimModel.B <- simModel(CFA.Model.B)

# The actual number of replications should be greater than 10.
Output.A.A <- simResult(10, SimData.A, SimModel.A)
Output.A.B <- simResult(10, SimData.A, SimModel.B)
Output.B.A <- simResult(10, SimData.B, SimModel.A)
Output.B.B <- simResult(10, SimData.B, SimModel.B)

getPowerFitNonNested(Output.B.A, Output.B.B, dat1Mod1=Output.A.A, dat1Mod2=Output.A.B)
getPowerFitNonNested(Output.B.A, Output.B.B, cutoff=c(AIC=0, BIC=0))

#################### pValue

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

#################### pValueNested

library(lavaan)

LY <- matrix(1, 4, 2)
LY[,2] <- 0:3
PS <- matrix(NA, 2, 2)
TY <- rep(0, 4)
AL <- rep(NA, 2)
TE <- diag(NA, 4)
linearModel <- simParamCFA(LY=LY, PS=PS, TY=TY, AL=AL, TE=TE)

LY2 <- matrix(1, 4, 2)
LY2[,2] <- c(0, NA, NA, 3)
unconstrainModel <- simParamCFA(LY=LY2, PS=PS, TY=TY, AL=AL, TE=TE)

nested <- simModel(linearModel, indLab=paste("t", 1:4, sep=""))
parent <- simModel(unconstrainModel, indLab=paste("t", 1:4, sep=""))

outNested <- run(nested, Demo.growth)
outParent <- run(parent, Demo.growth)

loadingMis <- matrix(0, 4, 2)
loadingMis[2:3, 2] <- NA
LYmis <- simMatrix(loadingMis, "runif(1, -0.1, 0.1)")
linearMis <- simMisspecCFA(LY=LYmis)

simNestedNested <- runFit(model=nested, data=Demo.growth, nRep=10, misspec=linearMis)
simNestedParent <- runFit(model=nested, data=Demo.growth, nRep=10, misspec=linearMis, analyzeModel=parent)

pValueNested(outNested, outParent, simNestedNested, simNestedParent)

#################### pValueNonNested

library(lavaan)
loading <- matrix(0, 11, 3)
loading[1:3, 1] <- NA
loading[4:7, 2] <- NA
loading[8:11, 3] <- NA
path.A <- matrix(0, 3, 3)
path.A[2:3, 1] <- NA
path.A[3, 2] <- NA
param.A <- simParamSEM(LY=loading, BE=path.A)

model.A <- simModel(param.A, indLab=c(paste("x", 1:3, sep=""), paste("y", 1:8, sep="")))
out.A <- run(model.A, PoliticalDemocracy)

path.B <- matrix(0, 3, 3)
path.B[1:2, 3] <- NA
path.B[1, 2] <- NA
param.B <- simParamSEM(LY=loading, BE=path.B)

model.B <- simModel(param.B, indLab=c(paste("x", 1:3, sep=""), paste("y", 1:8, sep="")))
out.B <- run(model.B, PoliticalDemocracy)

u2 <- simUnif(-0.2, 0.2)
loading.mis <- matrix(NA, 11, 3)
loading.mis[is.na(loading)] <- 0
LY.mis <- simMatrix(loading.mis, "u2")
misspec <- simMisspecSEM(LY=LY.mis)

output.A.A <- runFit(model.A, PoliticalDemocracy, 5, misspec=misspec)
output.A.B <- runFit(model.A, PoliticalDemocracy, 5, misspec=misspec, analyzeModel=model.B)
output.B.A <- runFit(model.B, PoliticalDemocracy, 5, misspec=misspec, analyzeModel=model.A)
output.B.B <- runFit(model.B, PoliticalDemocracy, 5, misspec=misspec)

# The output may contain some warnings here. When the number of replications increases (e.g., 1000), the warnings should disappear.
pValueNonNested(out.A, out.B, output.A.A, output.A.B, output.B.A, output.B.B)

#################### plotCutoff

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
# We make the examples running only 5 replications to save time.
# In reality, more replications are needed.
Output <- simResult(5, SimData, SimModel)
plotCutoff(Output, 0.05, usedFit=c("RMSEA", "SRMR", "CFI", "TLI"))

# Varying N
Output2 <- simResult(NULL, SimData, SimModel, n=seq(450, 500, 10))
plotCutoff(Output2, 0.05)

# Varying N and pmMCAR
Output3 <- simResult(NULL, SimData, SimModel, n=seq(450, 500, 10), pmMCAR=c(0, 0.05, 0.1, 0.15))
plotCutoff(Output3, 0.05)

#################### plotCutoffNested

n1 <- simNorm(0, 0.1)
u79 <- simUnif(0.7, 0.9)

loading.null <- matrix(0, 6, 1)
loading.null[1:6, 1] <- NA
LX.NULL <- simMatrix(loading.null, 0.7)
RPH.NULL <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model.NULL <- simSetCFA(LY = LX.NULL, RPS = RPH.NULL, RTE = RTD)

error.cor.mis <- matrix(NA, 6, 6)
diag(error.cor.mis) <- 1
RTD.Mis <- symMatrix(error.cor.mis, "n1")
CFA.Model.NULL.Mis <- simMisspecCFA(RTE = RTD.Mis)

loading.alt <- matrix(0, 6, 2)
loading.alt[1:3, 1] <- NA
loading.alt[4:6, 2] <- NA
LX.ALT <- simMatrix(loading.alt, 0.7)
latent.cor.alt <- matrix(NA, 2, 2)
diag(latent.cor.alt) <- 1
RPH.ALT <- symMatrix(latent.cor.alt, "u79")
CFA.Model.ALT <- simSetCFA(LY = LX.ALT, RPS = RPH.ALT, RTE = RTD)

SimData.NULL <- simData(CFA.Model.NULL, 500)

SimModel.NULL <- simModel(CFA.Model.NULL)
SimModel.ALT <- simModel(CFA.Model.ALT)

# The actual number of replications should be greater than 10.
Output.NULL.NULL <- simResult(10, SimData.NULL, SimModel.NULL)
Output.NULL.ALT <- simResult(10, SimData.NULL, SimModel.ALT)

plotCutoffNested(Output.NULL.NULL, Output.NULL.ALT, alpha=0.05)

#################### plotCutoffNonNested

n1 <- simNorm(0, 0.1)
u79 <- simUnif(0.7, 0.9)

loading.A <- matrix(0, 8, 2)
loading.A[1:3, 1] <- NA
loading.A[4:8, 2] <- NA
LX.A <- simMatrix(loading.A, 0.7)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, "u79")
RTD <- symMatrix(diag(8))
CFA.Model.A <- simSetCFA(LY = LX.A, RPS = RPH, RTE = RTD)

error.cor.mis <- matrix(NA, 8, 8)
diag(error.cor.mis) <- 1
RTD.Mis <- symMatrix(error.cor.mis, "n1")
CFA.Model.A.Mis <- simMisspecCFA(RTE = RTD.Mis)

loading.B <- matrix(0, 8, 2)
loading.B[1:4, 1] <- NA
loading.B[5:8, 2] <- NA
LX.B <- simMatrix(loading.B, 0.7)
CFA.Model.B <- simSetCFA(LY = LX.B, RPS = RPH, RTE = RTD)

SimData.A <- simData(CFA.Model.A, 500)
SimData.B <- simData(CFA.Model.B, 500)

SimModel.A <- simModel(CFA.Model.A)
SimModel.B <- simModel(CFA.Model.B)

# The actual number of replications should be greater than 10.
Output.A.A <- simResult(10, SimData.A, SimModel.A)
Output.A.B <- simResult(10, SimData.A, SimModel.B)
Output.B.A <- simResult(10, SimData.B, SimModel.A)
Output.B.B <- simResult(10, SimData.B, SimModel.B)

plotCutoffNonNested(Output.A.A, Output.A.B, Output.B.A, Output.B.B)
plotCutoffNonNested(Output.A.A, Output.A.B)
plotCutoffNonNested(Output.A.A, Output.A.B, onetailed=TRUE)

#################### plotPower

# Specify Sample Size by n
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- simMatrix(loading, 0.4)
RPH <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model <- simSetCFA(LY = LX, RPS = RPH, RTE = RTD)
SimData <- simData(CFA.Model, 500)
SimModel <- simModel(CFA.Model)
# We will use only 5 replications to save time.
# In reality, more replications are needed.

# Specify both sample size and percent missing completely at random
Output <- simResult(NULL, SimData, SimModel, n=seq(100, 200, 20), pmMCAR=c(0, 0.1, 0.2))
plotPower(Output, "LY1_1", contMCAR=FALSE)

#################### plotPowerFit

loading.null <- matrix(0, 6, 1)
loading.null[1:6, 1] <- NA
LX.NULL <- simMatrix(loading.null, 0.7)
RPH.NULL <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model.NULL <- simSetCFA(LY = LX.NULL, RPS = RPH.NULL, RTE = RTD)
SimData.NULL <- simData(CFA.Model.NULL, 500)
SimModel <- simModel(CFA.Model.NULL)
# We make the examples running only 5 replications to save time.
# In reality, more replications are needed.
Output.NULL <- simResult(5, SimData.NULL, SimModel)

loading.alt <- matrix(0, 6, 2)
loading.alt[1:3, 1] <- NA
loading.alt[4:6, 2] <- NA
LX.ALT <- simMatrix(loading.alt, 0.7)
latent.cor.alt <- matrix(NA, 2, 2)
diag(latent.cor.alt) <- 1
RPH.ALT <- symMatrix(latent.cor.alt, 0.5)
CFA.Model.ALT <- simSetCFA(LY = LX.ALT, RPS = RPH.ALT, RTE = RTD)
SimData.ALT <- simData(CFA.Model.ALT, 500)
Output.ALT <- simResult(5, SimData.ALT, SimModel)
plotPowerFit(Output.ALT, nullObject=Output.NULL, alpha=0.05, usedFit=c("RMSEA", "CFI", "TLI", "SRMR"))
Rule.of.thumb <- c(RMSEA=0.05, CFI=0.95, TLI=0.95, SRMR=0.06)
plotPowerFit(Output.ALT, cutoff=Rule.of.thumb, alpha=0.05, usedFit=c("RMSEA", "CFI", "TLI", "SRMR"))

Output.NULL2 <- simResult(NULL, SimData.NULL, SimModel, n=seq(50, 250, 25))
Output.ALT2 <- simResult(NULL, SimData.ALT, SimModel, n=seq(50, 250, 25))

plotPowerFit(Output.ALT2, nullObject=Output.NULL2, alpha=0.05, usedFit=c("RMSEA", "CFI", "TLI", "SRMR"))
plotPowerFit(Output.ALT2, cutoff=Rule.of.thumb, alpha=0.05, usedFit=c("RMSEA", "CFI", "TLI", "SRMR"))

#################### plotPowerFitNested

u2 <- simUnif(-0.2, 0.2)
n1 <- simNorm(0, 0.1)
u79 <- simUnif(0.7, 0.9)

loading.null <- matrix(0, 6, 1)
loading.null[1:6, 1] <- NA
LX.NULL <- simMatrix(loading.null, 0.7)
RPH.NULL <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model.NULL <- simSetCFA(LY = LX.NULL, RPS = RPH.NULL, RTE = RTD)

error.cor.mis <- matrix(NA, 6, 6)
diag(error.cor.mis) <- 1
RTD.Mis <- symMatrix(error.cor.mis, "rnorm(1,0,0.1)")
CFA.Model.NULL.Mis <- simMisspecCFA(RTE = RTD.Mis)

loading.alt <- matrix(0, 6, 2)
loading.alt[1:3, 1] <- NA
loading.alt[4:6, 2] <- NA
LX.ALT <- simMatrix(loading.alt, 0.7)
latent.cor.alt <- matrix(NA, 2, 2)
diag(latent.cor.alt) <- 1
RPH.ALT <- symMatrix(latent.cor.alt, 0.7)
CFA.Model.ALT <- simSetCFA(LY = LX.ALT, RPS = RPH.ALT, RTE = RTD)

# loading.alt.mis <- matrix(NA, 6, 2)
# loading.alt.mis[is.na(loading.alt)] <- 0
# LX.alt.mis <- simMatrix(loading.alt.mis, "runif(1,-.2,.2)")
# CFA.Model.alt.mis <- simMisspecCFA(LY = LX.alt.mis, RTE=RTD.Mis)

SimData.NULL <- simData(CFA.Model.NULL, 500)
SimData.ALT <- simData(CFA.Model.ALT, 500)

SimModel.NULL <- simModel(CFA.Model.NULL)
SimModel.ALT <- simModel(CFA.Model.ALT)

Output.NULL.NULL <- simResult(10, SimData.NULL, SimModel.NULL)
Output.ALT.NULL <- simResult(10, SimData.ALT, SimModel.NULL)
Output.NULL.ALT <- simResult(10, SimData.NULL, SimModel.ALT)
Output.ALT.ALT <- simResult(10, SimData.ALT, SimModel.ALT)

plotPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, nullNested=Output.NULL.NULL, nullParent=Output.NULL.ALT)
plotPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, nullNested=Output.NULL.NULL, nullParent=Output.NULL.ALT, usedFit="CFI")

Output.NULL.NULL2 <- simResult(NULL, SimData.NULL, SimModel.NULL, n=seq(50, 500, 50))
Output.ALT.NULL2 <- simResult(NULL, SimData.ALT, SimModel.NULL, n=seq(50, 500, 50))
Output.NULL.ALT2 <- simResult(NULL, SimData.NULL, SimModel.ALT, n=seq(50, 500, 50))
Output.ALT.ALT2 <- simResult(NULL, SimData.ALT, SimModel.ALT, n=seq(50, 500, 50))

plotPowerFitNested(Output.ALT.NULL2, Output.ALT.ALT2, nullNested=Output.NULL.NULL2, nullParent=Output.NULL.ALT2)

plotPowerFitNested(Output.ALT.NULL2, Output.ALT.ALT2, nullNested=Output.NULL.NULL2, nullParent=Output.NULL.ALT2, logistic=FALSE)

plotPowerFitNested(Output.ALT.NULL2, Output.ALT.ALT2, cutoff=c(CFI=-0.1), logistic=FALSE)

#################### plotPowerFitNonNested

n1 <- simNorm(0, 0.1)
u79 <- simUnif(0.7, 0.9)

loading.A <- matrix(0, 8, 2)
loading.A[1:3, 1] <- NA
loading.A[4:8, 2] <- NA
LX.A <- simMatrix(loading.A, 0.7)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, "u79")
RTD <- symMatrix(diag(8))
CFA.Model.A <- simSetCFA(LY = LX.A, RPS = RPH, RTE = RTD)

error.cor.mis <- matrix(NA, 8, 8)
diag(error.cor.mis) <- 1
RTD.Mis <- symMatrix(error.cor.mis, "n1")
CFA.Model.A.Mis <- simMisspecCFA(RTE = RTD.Mis)

loading.B <- matrix(0, 8, 2)
loading.B[1:4, 1] <- NA
loading.B[5:8, 2] <- NA
LX.B <- simMatrix(loading.B, 0.7)
CFA.Model.B <- simSetCFA(LY = LX.B, RPS = RPH, RTE = RTD)

SimData.A <- simData(CFA.Model.A, 500)
SimData.B <- simData(CFA.Model.B, 500)

SimModel.A <- simModel(CFA.Model.A)
SimModel.B <- simModel(CFA.Model.B)

# The actual number of replications should be greater than 10.
Output.A.A <- simResult(10, SimData.A, SimModel.A)
Output.A.B <- simResult(10, SimData.A, SimModel.B)
Output.B.A <- simResult(10, SimData.B, SimModel.A)
Output.B.B <- simResult(10, SimData.B, SimModel.B)

plotPowerFitNonNested(Output.B.A, Output.B.B, dat1Mod1=Output.A.A, dat1Mod2=Output.A.B)
plotPowerFitNonNested(Output.B.A, Output.B.B, cutoff=c(AIC=0, BIC=0))

#################### runFit

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
Output <- runFit(SimModel, HolzingerSwineford1939, 5, mis)
summary(Output)

out <- run(SimModel, HolzingerSwineford1939)
Output2 <- runFit(out, HolzingerSwineford1939, 5, mis)

# Bollen-Stine Bootstrap
Output3 <- runFit(out, HolzingerSwineford1939, 5, modelBoot=TRUE)

# Bollen-Stine Bootstrap with trivial misspecification
Output4 <- runFit(out, HolzingerSwineford1939, 5, mis, modelBoot=TRUE)

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

#################### simResult

loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- simMatrix(loading, 0.7)
RPH <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model <- simSetCFA(LY = LX, RPS = RPH, RTE = RTD)
SimData <- simData(CFA.Model, 500)
SimModel <- simModel(CFA.Model)
# We make the examples running only 5 replications to save time.
# In reality, more replications are needed.
Output <- simResult(5, SimData, SimModel)
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
# We make the examples running only 5 replications to save time.
# In reality, more replications are needed.
Output <- simResult(NULL, SimData, SimModel, n=seq(50, 100, 10))
summary(Output)

# Specify both sample size and percent missing completely at random
Output <- simResult(NULL, SimData, SimModel, n=seq(50, 100, 10), pmMCAR=c(0, 0.1, 0.2))
summary(Output)

# Use distribution object on sample size and percent completely at random
n <- simUnif(100, 500)
pmMCAR <- simUnif(0, 0.1)
Output <- simResult(5, SimData, SimModel, n=n, pmMCAR=pmMCAR)
