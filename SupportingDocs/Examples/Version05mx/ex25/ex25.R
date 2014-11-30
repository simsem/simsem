library(simsem)
library(OpenMx)

Avalues <- matrix(0, 12, 12)
Avalues[1:3, 10] <- c(1, 0.8, 1.2)
Avalues[4:6, 11] <- c(1, 0.8, 1.2)
Avalues[7:9, 12] <- c(1, 0.8, 1.2)
Afree <- matrix(FALSE, 12, 12)
Afree[1:3, 10] <- c(FALSE, TRUE, TRUE)
Afree[4:6, 11] <- c(FALSE, TRUE, TRUE)
Afree[7:9, 12] <- c(FALSE, TRUE, TRUE)
Alabels <- matrix(NA, 12, 12)
Alabels[1:3, 10] <- c(NA, "con1", "con2")
Alabels[4:6, 11] <- c(NA, "con1", "con2")
Alabels[7:9, 12] <- c(NA, "con1", "con2")

Svalues <- diag(c(rep(0.4, 9), 1, 1.2, 1.4))
Svalues[10, 11] <- Svalues[11, 10] <- 0.77
Svalues[10, 12] <- Svalues[12, 10] <- 0.58
Svalues[11, 12] <- Svalues[12, 11] <- 0.91
Svalues[1, 4] <- Svalues[4, 1] <- 0.08
Svalues[2, 5] <- Svalues[5, 2] <- 0.08
Svalues[3, 6] <- Svalues[6, 3] <- 0.08
Svalues[4, 7] <- Svalues[7, 4] <- 0.08
Svalues[5, 8] <- Svalues[8, 5] <- 0.08
Svalues[6, 9] <- Svalues[9, 6] <- 0.08
Svalues[1, 7] <- Svalues[7, 1] <- 0.016
Svalues[2, 8] <- Svalues[8, 2] <- 0.016
Svalues[3, 9] <- Svalues[9, 3] <- 0.016
Sfree <- matrix(FALSE, 12, 12)
diag(Sfree) <- TRUE
Sfree[10, 11] <- Sfree[11, 10] <- TRUE
Sfree[10, 12] <- Sfree[12, 10] <- TRUE
Sfree[11, 12] <- Sfree[12, 11] <- TRUE
Sfree[1, 4] <- Sfree[4, 1] <- TRUE
Sfree[2, 5] <- Sfree[5, 2] <- TRUE
Sfree[3, 6] <- Sfree[6, 3] <- TRUE
Sfree[4, 7] <- Sfree[7, 4] <- TRUE
Sfree[5, 8] <- Sfree[8, 5] <- TRUE
Sfree[6, 9] <- Sfree[9, 6] <- TRUE
Sfree[1, 7] <- Sfree[7, 1] <- TRUE
Sfree[2, 8] <- Sfree[8, 2] <- TRUE
Sfree[3, 9] <- Sfree[9, 3] <- TRUE
Fvalues <- cbind(diag(9), matrix(0, 9, 3))

MvaluesNested <- c(rep(c(0, -0.5, 0.5), 3), 0, 0.5, 1)
MfreeNested <- c(rep(c(FALSE, TRUE, TRUE), 3), rep(TRUE, 3))
MlabelsNested <- c(rep(c(NA, "con3", "con4"), 3), rep(NA, 3))

popNested <- mxModel("Strong Invariance Model",
    type="RAM",
    mxMatrix(type="Full", nrow=12, ncol=12, values=Avalues, free=Afree, labels=Alabels, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=12, ncol=12, values=Svalues, free=Sfree, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=9, ncol=12, free=FALSE, values=Fvalues, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=12, values=MvaluesNested, free=MfreeNested, labels=MlabelsNested, name="M"),
    mxExpectationRAM("A","S","F","M", dimnames=c(paste0("y", 1:9), "f1", "f2", "f3"))
)

MvaluesParent <- c(0, -0.5, 0.5, 0, 0, 0, 0, 0.5, -0.5, 0, 0.5, 1)
MfreeParent <- c(rep(c(FALSE, TRUE, TRUE), 3), rep(TRUE, 3))

popParent <- mxModel("Weak Invariance Model",
    type="RAM",
    mxMatrix(type="Full", nrow=12, ncol=12, values=Avalues, free=Afree, labels=Alabels, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=12, ncol=12, values=Svalues, free=Sfree, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=9, ncol=12, free=FALSE, values=Fvalues, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=12, values=MvaluesParent, free=MfreeParent, name="M"),
    mxExpectationRAM("A","S","F","M", dimnames=c(paste0("y", 1:9), "f1", "f2", "f3"))
)

outDatNestedModNested <- sim(NULL, n = 50:500, popNested, generate = popNested, mxFit=TRUE)
outDatNestedModParent <- sim(NULL, n = 50:500, popParent, generate = popNested, mxFit=TRUE)

anova(outDatNestedModNested, outDatNestedModParent)

cutoff <- getCutoffNested(outDatNestedModNested, outDatNestedModParent, nVal = 250)
plotCutoffNested(outDatNestedModNested, outDatNestedModParent, alpha = 0.05)

outDatParentModNested <- sim(NULL, n = 50:500, popNested, generate = popParent, mxFit=TRUE)
outDatParentModParent <- sim(NULL, n = 50:500, popParent, generate = popParent, mxFit=TRUE)

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
