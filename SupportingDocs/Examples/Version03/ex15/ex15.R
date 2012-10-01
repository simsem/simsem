library(simsem)
library(lavaan)

loading <- matrix(0, 11, 3)
loading[1:3, 1] <- NA
loading[4:7, 2] <- NA
loading[8:11, 3] <- NA

path <- matrix(0, 3, 3)
path[2:3, 1] <- NA
path[3, 2] <- NA

param <- estmodel(LY=loading, BE=path, modelType="SEM", indLab=c(paste("x", 1:3, sep=""), paste("y", 1:8, sep="")), facLab=c("ind60", "dem60", "dem65"))

usedData <- imposeMissing(PoliticalDemocracy, pmMCAR=0.03)
mioption <- miss(m=5)
out <- analyze(param, usedData, miss=mioption)

loading.mis <- matrix("runif(1, -0.2, 0.2)", 11, 3)
loading.mis[is.na(loading)] <- 0
datamodel <- model.lavaan(out, std=TRUE, LY=loading.mis)

misstemplate <- miss(logical=is.na(usedData), m=5, ignoreCols="group")
output <- sim(1000, n=nrow(PoliticalDemocracy), datamodel, miss=misstemplate)
pValue(out, output)
