library(simsem)

loading <- matrix(0, 9, 3)
loading[1, 1] <- 1
loading[2:3, 1] <- NA
loading[4, 2] <- 1
loading[5:6, 2] <- NA
loading[7, 3] <- 1
loading[8:9, 3] <- NA
LY <- simMatrix(loading, "runif(1, 0.5, 1.5)")

facCor <- matrix(NA, 3, 3)
diag(facCor) <- 1
facCorVal <- diag(3)
facCorVal[1, 2] <- facCorVal[2, 1] <- 0.7
facCorVal[2, 3] <- facCorVal[3, 2] <- 0.7
facCorVal[1, 3] <- facCorVal[3, 1] <- 0.49
RPS <- symMatrix(facCor, facCorVal)

VE <- simVector(rep(NA, 3), c(1, 1.2, 1.4))

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
RTE <- symMatrix(error, errorVal)

VTE <- simVector(rep(NA, 9), 0.4)

TY <- simVector(c(0, NA, NA, 0, NA, NA, 0, NA, NA), "runif(1, -0.5, 0.5)")

AL <- simVector(rep(NA, 3), c(0, 0.5, 1))

longCFA <- simSetCFA(LY=LY, RPS=RPS, VE=VE, RTE=RTE, VTE=VTE, TY=TY, AL=AL)

con1 <- matrix(0, 3, 2)
con1[1,] <- c(2, 1)
con1[2,] <- c(5, 2)
con1[3,] <- c(8, 3)
rownames(con1) <- rep("LY", 3)
con2 <- matrix(0, 3, 2)
con2[1,] <- c(3, 1)
con2[2,] <- c(6, 2)
con2[3,] <- c(9, 3)
rownames(con2) <- rep("LY", 3)
con3 <- matrix(c(2, 5, 8), ncol=1)
rownames(con3) <- rep("TY", 3)
con4 <- matrix(c(3, 6, 9), ncol=1)
rownames(con4) <- rep("TY", 3)

equalCon1 <- simEqualCon(con1, con2, modelType="CFA")
equalCon2 <- simEqualCon(con1, con2, con3, con4, modelType="CFA")

# Trivial misspecification
loadingMis <- matrix(0, 9, 3)
loadingMis[2:3, 1] <- NA
loadingMis[5:6, 2] <- NA
loadingMis[8:9, 3] <- NA
LYMis <- simMatrix(loadingMis, "runif(1, -0.1, 0.1)") 

TYMis <- simVector(c(0, NA, NA, 0, NA, NA, 0, NA, NA), "runif(1, -0.1, 0.1)")

longCFAMis <- simMisspecCFA(LY=LYMis, TY=TYMis)

datNested <- simData(longCFA, 200, misspec=longCFAMis, equalCon=equalCon2)
datParent <- simData(longCFA, 200, misspec=longCFAMis, equalCon=equalCon1)

modNested <- simModel(longCFA, equalCon=equalCon2)
modParent <- simModel(longCFA, equalCon=equalCon1)

outDatNestedModNested <- simResult(NULL, datNested, modNested, n=50:500)
outDatNestedModParent <- simResult(NULL, datNested, modParent, n=50:500)

anova(outDatNestedModNested, outDatNestedModParent)

cutoff <- getCutoffNested(outDatNestedModNested, outDatNestedModParent, nVal=250)
plotCutoffNested(outDatNestedModNested, outDatNestedModParent, alpha=0.05)

outDatParentModNested <- simResult(NULL, datParent, modNested, n=50:500)
outDatParentModParent <- simResult(NULL, datParent, modParent, n=50:500)

anova(outDatParentModNested, outDatParentModParent)

getPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent, nVal=250)
getPowerFitNested(outDatParentModNested, outDatParentModParent, cutoff=cutoff, nVal=250)

plotPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent)
plotPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent, usedFit="RMSEA")
plotPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent, logistic=FALSE)

cutoff2 <- c(Chi=3.84, CFI=-0.01)
getPowerFitNested(outDatParentModNested, outDatParentModParent, cutoff=cutoff2, nVal=250, condCutoff=FALSE)
plotPowerFitNested(outDatParentModNested, outDatParentModParent, cutoff=cutoff2)
plotPowerFitNested(outDatParentModNested, outDatParentModParent, cutoff=cutoff2, logistic=FALSE)
plotPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent, cutoff=cutoff2, logistic=FALSE)
