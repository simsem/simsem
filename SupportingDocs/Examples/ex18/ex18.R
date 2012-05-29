library(simsem)

loading <- matrix(0, 5, 3)
loading[1,1] <- 1
loading[2:5,2] <- 1
loading[2:5,3] <- 0:3
LY <- simMatrix(loading)

facMean <- rep(NA, 3)
facMeanVal <- c(0.5, 5, 2)
AL <- simVector(facMean, facMeanVal)

facVar <- rep(NA, 3)
facVarVal <- c(0.25, 1, 0.25)
VPS <- simVector(facVar, facVarVal)

facCor <- diag(3)
facCor[2,3] <- NA
facCor[3,2] <- NA
RPS <- symMatrix(facCor, 0.5)

VTE <- simVector(c(0, rep(NA, 4)), 1.2)

RTE <- symMatrix(diag(5))

TY <- simVector(rep(0, 5))

path <- matrix(0, 3, 3)
path[2,1] <- NA
path[3,1] <- NA
pathVal <- matrix(0, 3, 3)
pathVal[2,1] <- 0.5
pathVal[3,1] <- 0.1
BE <- simMatrix(path, pathVal)

LCA.Model <- simSetSEM(LY=LY, RPS=RPS, VPS=VPS, AL=AL, VTE=VTE, RTE=RTE, TY=TY, BE=BE)

u1 <- simUnif(-0.1, 0.1)

loading.trivial <- matrix(0, 5, 3)
loading.trivial[3:4, 3] <- NA
loading.mis <- simMatrix(loading.trivial, "u1")

LCA.Mis <- simMisspecSEM(LY = loading.mis)

group <- simBinom(1, 0.5)
n01 <- simNorm(0, 1)
facDist <- simDataDist(group, n01, n01, keepScale=c(FALSE, TRUE, TRUE))

datTemplate <- simData(LCA.Model, 300, LCA.Mis, sequential=TRUE, facDist=facDist)
model <- simModel(LCA.Model)

Output <- simResult(NULL, datTemplate, model, n=seq(50, 500, 5), pmMCAR=seq(0, 0.4, 0.1))

plotCutoff(Output, 0.05)
getCutoff(Output, 0.05, nVal = 200, pmMCARval=0)
getCutoff(Output, 0.05, nVal = 300, pmMCARval=0.33)	

Cpow <- getPower(Output)
Cpow2 <- getPower(Output, nVal = 200, pmMCARval=0.35)
findPower(Cpow, "N", 0.80)
findPower(Cpow, "MCAR", 0.80)
plotPower(Output, powerParam=c("BE2_1", "BE3_1"))
