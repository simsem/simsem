# Assess global performance

library(devtools)
load_all("../../../simsem")

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

data <- run(SimData)

SimModel <- simModel(CFA.Model)

## SimMissing <- simMissing(pmMCAR=0.1, numImps=5)
context("Simple Simulation")
a <- system.time(
Output <- simResult(200, SimData, SimModel))
cat(paste(a[[1]],"\n",sep=""))


## Output <- simResult(100, SimData, SimModel, SimMissing, multicore=TRUE)
## getCutoff(Output, 0.05)
## plotCutoff(Output, 0.05)
## summaryParam(Output)
