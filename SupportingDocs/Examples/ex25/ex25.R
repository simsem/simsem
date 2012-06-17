library(simsem)

path <- matrix(0, 5, 5)
path[2, 1] <- NA
path[3, 2] <- NA
path[4, 3] <- NA
path[5, 4] <- NA
BE <- simMatrix(path, "runif(1, 0.3, 0.7)")

residual <- diag(5)
RPS <- symMatrix(residual)

pathModel <- simSetPath(RPS = RPS, BE = BE)

con <- matrix(0, 4, 2)
con[1,] <- c(2, 1)
con[2,] <- c(3, 2)
con[3,] <- c(4, 3)
con[4,] <- c(5, 4)
rownames(con) <- rep("BE", 4)
equalCon <- simEqualCon(con, modelType="Path")

pathMis <- matrix(0, 5, 5)
pathMis[2:5, 1] <- NA
pathMis[3:5, 2] <- NA
pathMis[4:5, 3] <- NA
pathMis[5, 4] <- NA
BEMis <- simMatrix(pathMis, "rnorm(1, 0, 0.05)")

pathModelMis <- simMisspecPath(BE = BEMis)

datNested <- simData(pathModel, 200, misspec=pathModelMis, equalCon=equalCon)
datParent <- simData(pathModel, 200, misspec=pathModelMis)

modNested <- simModel(pathModel, equalCon=equalCon)
modParent <- simModel(pathModel)

outDatNestedModNested <- simResult(NULL, datNested, modNested, n=50:500, pmMCAR=seq(0, 0.3, 0.1))
outDatNestedModParent <- simResult(NULL, datNested, modParent, n=50:500, pmMCAR=seq(0, 0.3, 0.1))

anova(outDatNestedModNested, outDatNestedModParent)

cutoff <- getCutoffNested(outDatNestedModNested, outDatNestedModParent, nVal=250, pmMCARval=0.2)
plotCutoffNested(outDatNestedModNested, outDatNestedModParent, alpha=0.05)

outDatParentModNested <- simResult(NULL, datParent, modNested, n=50:500, pmMCAR=seq(0, 0.3, 0.1))
outDatParentModParent <- simResult(NULL, datParent, modParent, n=50:500, pmMCAR=seq(0, 0.3, 0.1))

anova(outDatParentModNested, outDatParentModParent)

getPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent, nVal=250, pmMCARval=0.2)
getPowerFitNested(outDatParentModNested, outDatParentModParent, cutoff=cutoff, nVal=250, pmMCARval=0.2)

plotPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent)
plotPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent, usedFit="RMSEA")
plotPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent, useContour=FALSE)

cutoff2 <- c(Chi=3.84, CFI=-0.01)
getPowerFitNested(outDatParentModNested, outDatParentModParent, cutoff=cutoff2, nVal=250, pmMCARval=0.2, condCutoff=FALSE)
plotPowerFitNested(outDatParentModNested, outDatParentModParent, cutoff=cutoff2)
plotPowerFitNested(outDatParentModNested, outDatParentModParent, cutoff=cutoff2, useContour=FALSE)
