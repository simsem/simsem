library(simsem)
library(lavaan)
loading <- matrix(0, 9, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
model <- simParamCFA(LY=loading)
analyzeModel <- simModel(model, indLab=paste("x", 1:9, sep=""))
out <- run(analyzeModel, HolzingerSwineford1939)
summary(out)

simOut1 <- runFit(model=analyzeModel, data=HolzingerSwineford1939, nRep=1000)
getCutoff(simOut1, alpha=0.05)
pValue(out, simOut1)

loadingMis2 <- matrix(0, 9, 3)
loadingMis2[1,2] <- NA
loadingMis2[4,3] <- NA
LYMis2 <- simMatrix(loadingMis2, 0.3)
misspec2 <- simMisspecCFA(LY=LYMis2, misBeforeFill=FALSE)
simOut2 <- runFit(model=analyzeModel, data=HolzingerSwineford1939, nRep=1000, misspec=misspec2) 
getCutoff(simOut2, alpha=0.05)
pValue(out, simOut2)

loadingMis3 <- matrix(0, 9, 3)
loadingMis3[6,1] <- NA
loadingMis3[9,2] <- NA
LYMis3 <- simMatrix(loadingMis3, 0.3)
misspec3 <- simMisspecCFA(LY=LYMis3, misBeforeFill=FALSE)
simOut3 <- runFit(model=analyzeModel, data=HolzingerSwineford1939, nRep=1000, misspec=misspec3) 
getCutoff(simOut3, alpha=0.05)
pValue(out, simOut3)

u3 <- simUnif(-0.3, 0.3)
loadingMis4 <- matrix(0, 9, 3)
loadingMis4[4:9, 1] <- NA
loadingMis4[c(1:3, 7:9),2] <- NA
loadingMis4[1:6,3] <- NA
LYMis4 <- simMatrix(loadingMis4, "u3")
misspec4 <- simMisspecCFA(LY=LYMis4, misBeforeFill=FALSE)
simOut4 <- runFit(model=analyzeModel, data=HolzingerSwineford1939, nRep=1000, misspec=misspec4) 
getCutoff(simOut4, alpha=0.05)
pValue(out, simOut4)

n3 <- simNorm(0, 0.15)
loadingMis5 <- matrix(0, 9, 3)
loadingMis5[4:9, 1] <- NA
loadingMis5[c(1:3, 7:9),2] <- NA
loadingMis5[1:6,3] <- NA
LYMis5 <- simMatrix(loadingMis5, "n3")
misspec5 <- simMisspecCFA(LY=LYMis5, misBeforeFill=FALSE)
simOut5 <- runFit(model=analyzeModel, data=HolzingerSwineford1939, nRep=1000, misspec=misspec5) 
getCutoff(simOut5, alpha=0.05)
pValue(out, simOut5)

u3 <- simUnif(-0.3, 0.3)
loadingMis6 <- matrix(0, 9, 3)
loadingMis6[4:9, 1] <- NA
loadingMis6[c(1:3, 7:9),2] <- NA
loadingMis6[1:6,3] <- NA
LYMis6 <- simMatrix(loadingMis6, "u3")
misspec6 <- simMisspecCFA(LY=LYMis6, optMisfit="max", numIter=100, misBeforeFill=FALSE)
simOut6 <- runFit(model=analyzeModel, data=HolzingerSwineford1939, nRep=1000, misspec=misspec6) 
getCutoff(simOut6, alpha=0.05)
pValue(out, simOut6)

u1 <- simUnif(-0.1, 0.1)
loadingMis7 <- matrix(0, 9, 3)
loadingMis7[4:9, 1] <- NA
loadingMis7[c(1:3, 7:9),2] <- NA
loadingMis7[1:6,3] <- NA
LYMis7 <- simMatrix(loadingMis7, "u1")
misspec7 <- simMisspecCFA(LY=LYMis7, misfitBound=c(0.02, 0.05), numIter=200, misBeforeFill=FALSE)
simOut7 <- runFit(model=analyzeModel, data=HolzingerSwineford1939, nRep=1000, misspec=misspec7) 
getCutoff(simOut7, alpha=0.05)
pValue(out, simOut7)

# To be rejected
u69 <- simUnif(0.6, 0.9)
loadingMisAlt <- matrix(0, 9, 3)
loadingMisAlt[4, 1] <- NA
loadingMisAlt[7, 2] <- NA
loadingMisAlt[1, 3] <- NA
LYMisAlt <- simMatrix(loadingMisAlt, "u69")
misspecAlt <- simMisspecCFA(LY=LYMisAlt, optMisfit="min", numIter=100, misBeforeFill=FALSE)
simOutAlt <- runFit(model=analyzeModel, data=HolzingerSwineford1939, nRep=1000, misspec=misspecAlt) 
getPowerFit(simOutAlt, nullObject=simOut1) 
getPowerFit(simOutAlt, nullObject=simOut2) 
getPowerFit(simOutAlt, nullObject=simOut3) 
getPowerFit(simOutAlt, nullObject=simOut4) 
getPowerFit(simOutAlt, nullObject=simOut5) 
getPowerFit(simOutAlt, nullObject=simOut6) 
getPowerFit(simOutAlt, nullObject=simOut7) 

param1 <- runFitParam(analyzeModel, data=HolzingerSwineford1939)
param2 <- runFitParam(analyzeModel, data=HolzingerSwineford1939, misspec=misspec2)
param3 <- runFitParam(analyzeModel, data=HolzingerSwineford1939, misspec=misspec3)
param4 <- runFitParam(analyzeModel, data=HolzingerSwineford1939, misspec=misspec4)
param5 <- runFitParam(analyzeModel, data=HolzingerSwineford1939, misspec=misspec5)
param6 <- runFitParam(analyzeModel, data=HolzingerSwineford1939, misspec=misspec6)
param7 <- runFitParam(analyzeModel, data=HolzingerSwineford1939, misspec=misspec7)
paramAlt <- runFitParam(analyzeModel, data=HolzingerSwineford1939, misspec=misspecAlt)

summary(param4)
summaryParam(param4)
summaryMisspec(param4)
summaryFit(param4)
plotMisfit(param4, misParam="LY9_1")
