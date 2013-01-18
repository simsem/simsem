library(simsem)

loadingNested <- matrix(0, 9, 3)
loadingNested[1, 1] <- 1
loadingNested[2:3, 1] <- c("con1", "con2")
loadingNested[4, 2] <- 1
loadingNested[5:6, 2] <- c("con1", "con2")
loadingNested[7, 3] <- 1
loadingNested[8:9, 3] <- c("con1", "con2")
loadingMis <- matrix(0, 9, 3)
loadingMis[2:3, 1] <- "runif(1, -0.1, 0.1)"
loadingMis[5:6, 2] <- "runif(1, -0.1, 0.1)"
loadingMis[8:9, 3] <- "runif(1, -0.1, 0.1)"
LYnested <- bind(loadingNested, "runif(1, 0.5, 1.5)", misspec = loadingMis)

facCor <- matrix(NA, 3, 3)
diag(facCor) <- 1
facCorVal <- diag(3)
facCorVal[1, 2] <- facCorVal[2, 1] <- 0.7
facCorVal[2, 3] <- facCorVal[3, 2] <- 0.7
facCorVal[1, 3] <- facCorVal[3, 1] <- 0.49
RPS <- binds(facCor, facCorVal)

VE <- bind(rep(NA, 3), c(1, 1.2, 1.4))

error <- diag(9)
error[1, 4] <- error[4, 7] <- error[4, 1] <- error[7, 4] <- NA
error[2, 5] <- error[5, 8] <- error[5, 2] <- error[8, 5] <- NA
error[3, 6] <- error[6, 9] <- error[6, 3] <- error[9, 6] <- NA
error[1, 7] <- error[7, 1] <- NA
error[2, 8] <- error[8, 2] <- NA
error[3, 9] <- error[9, 3] <- NA
errorVal <- diag(9)
errorVal[1, 4] <- errorVal[4, 7] <- errorVal[4, 1] <- errorVal[7, 4] <- 0.2
errorVal[2, 5] <- errorVal[5, 8] <- errorVal[5, 2] <- errorVal[8, 5] <- 0.2
errorVal[3, 6] <- errorVal[6, 9] <- errorVal[6, 3] <- errorVal[9, 6] <- 0.2
errorVal[1, 7] <- errorVal[7, 1] <- 0.04
errorVal[2, 8] <- errorVal[8, 2] <- 0.04
errorVal[3, 9] <- errorVal[9, 3] <- 0.04
RTE <- binds(error, errorVal)

VTE <- bind(rep(NA, 9), 0.4)

longNested <- model(LY=LYnested, RPS=RPS, VE=VE, RTE=RTE, VTE=VTE, modelType = "CFA")

loadingParent <- matrix(0, 9, 3)
loadingParent[1, 1] <- 1
loadingParent[2:3, 1] <- NA
loadingParent[4, 2] <- 1
loadingParent[5:6, 2] <- NA
loadingParent[7, 3] <- 1
loadingParent[8:9, 3] <- NA
loadingMis <- matrix(0, 9, 3)
loadingMis[2:3, 1] <- "runif(1, -0.1, 0.1)"
loadingMis[5:6, 2] <- "runif(1, -0.1, 0.1)"
loadingMis[8:9, 3] <- "runif(1, -0.1, 0.1)"
LYparent <- bind(loadingParent, "runif(1, 0.5, 1.5)", misspec = loadingMis)

longParent <- model(LY=LYparent, RPS=RPS, VE=VE, RTE=RTE, VTE=VTE, modelType = "CFA")

outDatNestedModNested <- sim(1000, n = 200, longNested, generate = longNested)
outDatNestedModParent <- sim(1000, n = 200, longParent, generate = longNested)

anova(outDatNestedModNested, outDatNestedModParent)
cutoff <- getCutoffNested(outDatNestedModNested, outDatNestedModParent)
plotCutoffNested(outDatNestedModNested, outDatNestedModParent, alpha=0.05)

outDatParentModNested <- sim(1000, n = 200, longNested, generate = longParent)
outDatParentModParent <- sim(1000, n = 200, longParent, generate = longParent)

anova(outDatParentModNested, outDatParentModParent)

getPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent)
getPowerFitNested(outDatParentModNested, outDatParentModParent, cutoff=cutoff)
plotPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent)
plotPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent, usedFit="RMSEA")

cutoff2 <- c(Chi = 3.84, CFI = -0.01)
getPowerFitNested(outDatParentModNested, outDatParentModParent, cutoff=cutoff2)
plotPowerFitNested(outDatParentModNested, outDatParentModParent, cutoff=cutoff2)
plotPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent, cutoff=cutoff2)
