library(simsem)

n1 <- simNorm(0, 0.1)

loading.null <- matrix(0, 8, 2)
loading.null[1:5, 1] <- NA
loading.null[6:8, 2] <- NA
LX.NULL <- simMatrix(loading.null, 0.7)
latent.cor.null <- matrix(NA, 2, 2)
diag(latent.cor.null) <- 1
RPH <- symMatrix(latent.cor.null, 0.5)
RTD <- symMatrix(diag(8))
CFA.Model.NULL <- simSetCFA(LY = LX.NULL, RPS = RPH, RTE = RTD)

loading.alt <- matrix(0, 8, 2)
loading.alt[1:4, 1] <- NA
loading.alt[5:8, 2] <- NA
LX.ALT <- simMatrix(loading.alt, 0.7)
CFA.Model.ALT <- simSetCFA(LY = LX.ALT, RPS = RPH, RTE = RTD)

error.cor.mis <- matrix(NA, 8, 8)
diag(error.cor.mis) <- 1
RTD.Mis <- symMatrix(error.cor.mis, "n1")
CFA.Model.Mis <- simMisspecCFA(RTE = RTD.Mis)

SimData.NULL <- simData(CFA.Model.NULL, 500, misspec=CFA.Model.Mis)
SimData.ALT <- simData(CFA.Model.ALT, 500, misspec=CFA.Model.Mis)

SimModel <- simModel(CFA.Model.NULL)

Output.NULL <- simResult(NULL, SimData.NULL, SimModel, n=25:500)
Output.ALT <- simResult(NULL, SimData.ALT, SimModel, n=25:500)

cutoff <- getCutoff(Output.NULL, alpha=0.05, nVal=250)
plotCutoff(Output.NULL, alpha=0.05)
getPowerFit(Output.ALT, nullObject=Output.NULL, alpha=0.05, nVal=250)
getPowerFit(Output.ALT, cutoff=cutoff, nVal=250, condCutoff=TRUE)
plotPowerFit(Output.ALT, Output.NULL, alpha=0.05)
plotPowerFit(Output.ALT, Output.NULL, alpha=0.05, logistic=FALSE)

cutoff2 <- c(RMSEA = 0.05, CFI = 0.95, TLI = 0.95, SRMR = 0.06)
getPowerFit(Output.ALT, cutoff=cutoff2, nVal=250, condCutoff=FALSE)
plotPowerFit(Output.ALT, cutoff=cutoff2)
plotPowerFit(Output.ALT, cutoff=cutoff2, logistic=FALSE)
