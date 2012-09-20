library(simsem)
library(lavaan)

loading <- matrix(0, 9, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
cfamodel <- estmodel(LY=loading, modelType="CFA", indLab=paste("x", 1:9, sep=""))
out <- analyze(cfamodel, HolzingerSwineford1939)

datamodel.nomis <- model.lavaan(out, std=TRUE)
output.nomis <- sim(1000, n=nrow(HolzingerSwineford1939), datamodel.nomis)
plotCutoff(output.nomis, 0.05)
pValue(out, output.nomis)

loading.mis <- matrix("runif(1, -0.2, 0.2)", 9, 3)
loading.mis[is.na(loading)] <- 0
datamodel.mis <- model.lavaan(out, std=TRUE, LY=loading.mis)

output.mis <- sim(1000, n=nrow(HolzingerSwineford1939), datamodel.mis)
plotCutoff(output.mis, 0.05)
pValue(out, output.mis)
