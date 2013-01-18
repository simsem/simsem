library(simsem)

pathNested <- matrix(0, 5, 5)
pathNested[2, 1] <- "con"
pathNested[3, 2] <- "con"
pathNested[4, 3] <- "con"
pathNested[5, 4] <- "con"
pathMis <- matrix(0, 5, 5)
pathMis[2:5, 1] <- "rnorm(1, 0, 0.05)"
pathMis[3:5, 2] <- "rnorm(1, 0, 0.05)"
pathMis[4:5, 3] <- "rnorm(1, 0, 0.05)"
pathMis[5, 4] <- "rnorm(1, 0, 0.05)"
BEnested <- bind(pathNested, "runif(1, 0.3, 0.7)", misspec = pathMis)

residual <- diag(5)
RPS <- binds(residual)

modelNested <- model(RPS = RPS, BE = BEnested, modelType="Path")

pathParent <- matrix(0, 5, 5)
pathParent[2, 1] <- NA
pathParent[3, 2] <- NA
pathParent[4, 3] <- NA
pathParent[5, 4] <- NA
pathMis <- matrix(0, 5, 5)
pathMis[2:5, 1] <- "rnorm(1, 0, 0.05)"
pathMis[3:5, 2] <- "rnorm(1, 0, 0.05)"
pathMis[4:5, 3] <- "rnorm(1, 0, 0.05)"
pathMis[5, 4] <- "rnorm(1, 0, 0.05)"
BEparent <- bind(pathParent, "runif(1, 0.3, 0.7)", misspec = pathMis)

modelParent <- model(RPS = RPS, BE = BEparent, modelType="Path")

outDatNestedModNested <- sim(NULL, n=50:500, modelNested, generate = modelNested, pmMCAR=seq(0, 0.3, 0.1))
outDatNestedModParent <- sim(NULL, n=50:500, modelParent, generate = modelNested, pmMCAR=seq(0, 0.3, 0.1))

anova(outDatNestedModNested, outDatNestedModParent)

cutoff <- getCutoffNested(outDatNestedModNested, outDatNestedModParent, nVal=250, pmMCARval=0.2)
plotCutoffNested(outDatNestedModNested, outDatNestedModParent, alpha=0.05)

outDatParentModNested <- sim(NULL, n=50:500, modelNested, generate = modelParent, pmMCAR=seq(0, 0.3, 0.1))
outDatParentModParent <- sim(NULL, n=50:500, modelParent, generate = modelParent, pmMCAR=seq(0, 0.3, 0.1))

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
