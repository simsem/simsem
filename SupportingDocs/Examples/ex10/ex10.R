library(simsem)

u35 <- simUnif(0.3, 0.5)
u57 <- simUnif(0.5, 0.7)
u1 <- simUnif(-0.1, 0.1)
u3 <- simUnif(-0.3, 0.3)
n1 <- simNorm(0, 0.1)
n31 <- simNorm(0.3, 0.1)
u79 <- simUnif(0.7, 0.9)
chi5 <- simChisq(5)

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
loading <- matrix(0, 12, 4)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
loading[10:12, 4] <- NA
LY <- simMatrix(loading, "u79")
RTE <- symMatrix(diag(12))
SEM.Model <- simSetSEM(RPS = RPS, BE = BE, LY = LY, RTE = RTE)

mis.path.BE <- matrix(0, 4, 4)
mis.path.BE[4, 1:2] <- NA
mis.BE <- simMatrix(mis.path.BE, "u1")
mis.loading <- matrix(NA, 12, 4)
mis.loading[is.na(loading)] <- 0
mis.LY <- simMatrix(mis.loading, "u3")
mis.error.cor <- matrix(NA, 12, 12)
diag(mis.error.cor) <- 0
mis.RTE <- symMatrix(mis.error.cor, "n1")
SEM.Mis.Model <- simMisspecSEM(BE = mis.BE, LY = mis.LY, RTE = mis.RTE)

facDist <- simDataDist(chi5, chi5, n1, n1)
dataTemplate <- simData(SEM.Model, 500, SEM.Mis.Model, sequential=TRUE, facDist=facDist)
modelTemplate <- simModel(SEM.Model, estimator="mlr")
simOut <- simResult(1000, dataTemplate, modelTemplate)
getCutoff(simOut, 0.05)
plotCutoff(simOut, 0.05)
summaryParam(simOut)
