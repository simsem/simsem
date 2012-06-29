library(simsem)

n05 <- simNorm(0, 0.05)

path.null <- matrix(0, 5, 5)
path.null[2, 1] <- NA
path.null[3, 2] <- NA
path.null[4, 3] <- NA
path.null[5, 4] <- NA
BE.null <- simMatrix(path.null, 0.4)

residual <- diag(5)
RPS <- symMatrix(residual)

path.model.null <- simSetPath(RPS = RPS, BE = BE.null)

path.null.mis <- matrix(0, 5, 5)
path.null.mis[3:5, 1] <- NA
path.null.mis[4:5, 2] <- NA
path.null.mis[5, 3] <- NA
BE.null.mis <- simMatrix(path.null.mis, "n05")

path.model.null.mis <- simMisspecPath(BE = BE.null.mis)

path.alt <- matrix(0, 5, 5)
path.alt[2:3, 1] <- NA
path.alt[4, 2:3] <- NA
path.alt[5, 4] <- NA
BE.alt <- simMatrix(path.alt, 0.4)

path.model.alt <- simSetPath(RPS = RPS, BE = BE.alt)

path.alt.mis <- matrix(0, 5, 5)
path.alt.mis[4:5, 1] <- NA
path.alt.mis[5, 2:3] <- NA
BE.alt.mis <- simMatrix(path.alt.mis, "n05")

path.model.alt.mis <- simMisspecPath(BE = BE.alt.mis)

SimData.NULL <- simData(path.model.null, 500, misspec=path.model.null.mis)
SimData.ALT <- simData(path.model.alt, 500, misspec=path.model.alt.mis)

SimModel <- simModel(path.model.null)

Output.NULL <- simResult(NULL, SimData.NULL, SimModel, n=25:500, pmMCAR=seq(0, 0.3, 0.1))
Output.ALT <- simResult(NULL, SimData.ALT, SimModel, n=25:500, pmMCAR=seq(0, 0.3, 0.1))

cutoff <- getCutoff(Output.NULL, alpha=0.05, nVal=250, pmMCARval = 0.2)
plotCutoff(Output.NULL, alpha=0.05)
getPowerFit(Output.ALT, nullObject=Output.NULL, alpha=0.05, nVal=250, pmMCARval = 0.2)
getPowerFit(Output.ALT, cutoff=cutoff, nVal=250, pmMCARval = 0.2, condCutoff=TRUE)
plotPowerFit(Output.ALT, Output.NULL, alpha=0.05)

cutoff2 <- c(RMSEA = 0.05, CFI = 0.95, TLI = 0.95, SRMR = 0.06)
getPowerFit(Output.ALT, cutoff=cutoff2, nVal=250, pmMCARval = 0.2, condCutoff=FALSE)
plotPowerFit(Output.ALT, cutoff=cutoff2)
