library(simsem)
library(lavaan)
loading <- matrix(0, 11, 3)
loading[1:3, 1] <- NA
loading[4:7, 2] <- NA
loading[8:11, 3] <- NA
path <- matrix(0, 3, 3)
path[2:3, 1] <- NA
path[3, 2] <- NA
param <- simParamSEM(LY=loading, BE=path)

missreal <- simMissing(pmMCAR=0.03)
usedData <- run(missreal, PoliticalDemocracy)

model <- simModel(param, indLab=c(paste("x", 1:3, sep=""), paste("y", 1:8, sep="")))
miss <- simMissing(numImps=5)
out <- run(model, usedData, miss)

u2 <- simUnif(-0.2, 0.2)
loading.mis <- matrix(NA, 11, 3)
loading.mis[is.na(loading)] <- 0
LY.mis <- simMatrix(loading.mis, "u2")
misspec <- simMisspecSEM(LY=LY.mis)
output <- runFit(model, usedData, 1000, misspec=misspec, missModel=miss)
plotCutoff(output, 0.05)
pValue(out, output)
