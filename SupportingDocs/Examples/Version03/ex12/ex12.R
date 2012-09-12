library(simsem)

u57 <- simUnif(0.5, 0.7)
u4 <- simUnif(-0.4, 0.4)
u35 <- simUnif(0.3, 0.5)
u2 <- simUnif(-0.2, 0.2)
n01 <- simNorm(0, 1)

loading <- matrix(0, 7, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LX <- simMatrix(loading, "u57")
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, "u35")
error.cor <- diag(7)
error.cor[1:6, 7] <- NA
error.cor[7, 1:6] <- NA
RTD <- symMatrix(error.cor, "u4")
VX <- simVector(rep(NA, 7), 1)
CFA.Model.Aux <- simSetCFA(LX = LX, RPH = RPH, RTD = RTD, VX = VX) 

mis.loading <- matrix(0, 7, 2)
mis.loading[1:3, 2] <- NA
mis.loading[4:6, 1] <- NA
mis.LY <- simMatrix(mis.loading, "u2")
CFA.Mis.Model <- simMisspecCFA(LY = mis.LY)

SimData <- simData(CFA.Model.Aux, 200, misspec = CFA.Mis.Model)

CFA.Model <- extract(CFA.Model.Aux, y=1:6)

SimMissing <- simMissing(pmMAR=0.1, cov=7, numImps=5, threshold=0.5)

SimModel <- simModel(CFA.Model)

Output <- simResult(1000, SimData, SimModel, SimMissing)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summaryParam(Output)
