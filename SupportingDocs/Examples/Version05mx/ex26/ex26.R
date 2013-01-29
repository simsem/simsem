library(simsem)
library(OpenMx)

AvaluesNull <- matrix(0, 5, 5)
AvaluesNull[2, 1] <- 0.4
AvaluesNull[3, 2] <- 0.4
AvaluesNull[4, 3] <- 0.4
AvaluesNull[5, 4] <- 0.4
AfreeNull <- matrix(FALSE, 5, 5)
AfreeNull[2, 1] <- TRUE
AfreeNull[3, 2] <- TRUE
AfreeNull[4, 3] <- TRUE
AfreeNull[5, 4] <- TRUE
AlabelsNull <- matrix(NA, 5, 5)
AlabelsNull[2, 1] <- "con"
AlabelsNull[3, 2] <- "con"
AlabelsNull[4, 3] <- "con"
AlabelsNull[5, 4] <- "con"

Svalues <- diag(c(1, rep(0.8, 4)))
Sfree <- matrix(FALSE, 5, 5)
diag(Sfree) <- TRUE
Fvalues <- diag(5)

popNull <- mxModel("Null Hypothesis Model",
    type="RAM",
    mxMatrix(type="Full", nrow=5, ncol=5, values=AvaluesNull, free=AfreeNull, labels=AlabelsNull, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=5, ncol=5, values=Svalues, free=Sfree, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=5, ncol=5, free=FALSE, values=Fvalues, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=5, values=rep(0, 5), free=rep(TRUE, 5), name="M"),
    mxRAMObjective("A","S","F","M", dimnames=paste0("y", 1:5))
)

AvaluesAlt <- matrix(0, 5, 5)
AvaluesAlt[2, 1] <- 0.4
AvaluesAlt[3, 2] <- 0.5
AvaluesAlt[4, 3] <- 0.3
AvaluesAlt[5, 4] <- 0.7
AfreeAlt <- matrix(FALSE, 5, 5)
AfreeAlt[2, 1] <- TRUE
AfreeAlt[3, 2] <- TRUE
AfreeAlt[4, 3] <- TRUE
AfreeAlt[5, 4] <- TRUE

popAlt <- mxModel("Alternative Hypothesis Model",
    type="RAM",
    mxMatrix(type="Full", nrow=5, ncol=5, values=AvaluesAlt, free=AfreeAlt, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=5, ncol=5, values=Svalues, free=Sfree, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=5, ncol=5, free=FALSE, values=Fvalues, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=5, values=rep(0, 5), free=rep(TRUE, 5), name="M"),
    mxRAMObjective("A","S","F","M", dimnames=paste0("y", 1:5))
)

outDatNestedModNested <- sim(NULL, n = 50:500, popNested, generate = popNested, pmMCAR=seq(0, 0.3, 0.1), mxFit = TRUE)
outDatNestedModParent <- sim(NULL, n = 50:500, popParent, generate = popNested, pmMCAR=seq(0, 0.3, 0.1), mxFit = TRUE)

anova(outDatNestedModNested, outDatNestedModParent)

cutoff <- getCutoffNested(outDatNestedModNested, outDatNestedModParent, nVal=250, pmMCARval=0.2)
plotCutoffNested(outDatNestedModNested, outDatNestedModParent, alpha=0.05)

outDatParentModNested <- sim(NULL, n = 50:500, popNested, generate = popParent, pmMCAR=seq(0, 0.3, 0.1), mxFit = TRUE)
outDatParentModParent <- sim(NULL, n = 50:500, popParent, generate = popParent, pmMCAR=seq(0, 0.3, 0.1), mxFit = TRUE)

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
