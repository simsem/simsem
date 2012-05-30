library(simsem)
library(lavaan)
loading <- matrix(0, 9, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
model <- simParamCFA(LY=loading)
SimModel <- simModel(model, indLab=paste("x", 1:9, sep=""))
out <- run(SimModel, HolzingerSwineford1939)

### Making result object without trivial model misspecification
#output <- runFit(SimModel, HolzingerSwineford1939, 1000)
#pValue(out, output)

u2 <- simUnif(-0.2, 0.2)
loading.mis <- matrix(NA, 9, 3)
loading.mis[is.na(loading)] <- 0
LY.mis <- simMatrix(loading.mis, "u2")
misspec <- simMisspecCFA(LY=LY.mis)
output2 <- runFit(SimModel, HolzingerSwineford1939, 1000, misspec=misspec)
plotCutoff(output2, 0.05)
pValue(out, output2)
