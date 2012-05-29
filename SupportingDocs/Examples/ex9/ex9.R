library(simsem)

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
LX <- simMatrix(loading, 0.7)

latent.cor <- matrix(NA, 3, 3)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, "u5")

error.cor <- matrix(0, 12, 12)
diag(error.cor) <- 1
RTD <- symMatrix(error.cor)

CFA.Model <- simSetCFA(LX = LX, RPH = RPH, RTD = RTD) 

loading.mis <- matrix(NA, 12, 3)
loading.mis[is.na(loading)] <- 0
LY.mis <- simMatrix(loading.mis, "u2")
CFA.model.mis <- simMisspecCFA(LY=LY.mis)

SimDataDist <- simDataDist(t2, t3, t4, t5, chi3, chi4, chi5, chi6, chi3, chi4, chi5, chi6, reverse=c(rep(FALSE, 8), rep(TRUE, 4)))
SimData <- simData(CFA.Model, 200, misspec=CFA.model.mis, indDist=SimDataDist)
SimModel <- simModel(CFA.Model, estimator="mlm")
Output <- simResult(1000, SimData, SimModel)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)
