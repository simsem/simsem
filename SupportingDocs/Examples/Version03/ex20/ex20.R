library(simsem)

u39 <- simUnif(0.3, 0.9)
u09 <- simUnif(0, 0.9)
loading <- matrix(0, 10, 2)
loading[1:6, 1] <- NA
loading[7:10, 2] <- NA
LY <- simMatrix(loading, "u39")

RPS <- symMatrix(diag(2))

RTE <- symMatrix(diag(10))

path <- matrix(0, 2, 2)
path[2, 1] <- NA
BE <- simMatrix(path, "u09")

latentReg <- simSetSEM(LY = LY, RPS = RPS, RTE = RTE, BE = BE)

SimData <- simData(latentReg, 500)
SimModel <- simModel(latentReg)
Output <- simResult(NULL, SimData, SimModel, n=25:500)
summary(Output)
plotCutoff(Output, 0.05)
getCutoff(Output, 0.05, n = 200)	

Cpow <- getPower(Output, contParam="BE2_1")
Cpow2 <- getPower(Output, contParam="BE2_1", nVal = 200, paramVal=seq(0.1, 0.9, 0.1))

targetVal <- list("BE2_1" = seq(0.1, 0.9, 0.1), "LY1_1" = c(0.5, 0.7))
Cpow3 <- getPower(Output, contParam=c("BE2_1", "LY1_1"), nVal = 200, paramVal=targetVal)

findPower(Cpow, 1, 0.80)
findPower(Cpow, 2, 0.80)

plotPower(Output, powerParam=c("BE2_1", "LY10_2"), contParam="BE2_1")
