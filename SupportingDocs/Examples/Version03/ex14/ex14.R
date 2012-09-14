library(simsem)

u79 <- simUnif(0.7, 0.9)
u5 <- simUnif(-0.5, 0.5)
n01 <- simNorm(0, 1)
c5 <- simChisq(5)

loading <- matrix(0, 5, 3)
loading[1:3, 1] <- NA
loading[4, 2] <- NA
loading[5, 3] <- NA
loadingVal <- matrix(0, 5, 3)
loadingVal[1:3, 1] <- "u79"
loadingVal[4, 2] <- 1
loadingVal[5, 3] <- 1
LY <- simMatrix(loading, loadingVal)

facCor <- diag(3)
facCor[2, 1] <- NA
facCor[1, 2] <- NA
RPS <- symMatrix(facCor, "u5")

path <- matrix(0, 3, 3)
path[3, 1] <- NA
path[3, 2] <- NA
BE <- simMatrix(path, "u5")

RTE <- symMatrix(diag(5))

VY <- simVector(c(NA, NA, NA, 0, 0), 1)

SEM.Model <- simSetSEM(LY=LY, RPS=RPS, BE=BE, RTE=RTE, VY=VY)

errorCorMis <- diag(5)
errorCorMis[1:3, 1:3] <- NA
errorCorMis <- diag(5)
RTE.mis <- symMatrix(errorCorMis, n01)

SEM.Model.Mis <- simMisspecSEM(RTE=RTE.mis)

facDist <- simDataDist(n01, c5, n01)

SimData <- simData(SEM.Model, 200, misspec=SEM.Model.Mis, sequential=TRUE, facDist=facDist)
SimModel <- simModel(SEM.Model, estimator="mlm")
Output <- simResult(1000, SimData, SimModel)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summaryParam(Output)
