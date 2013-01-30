library(simsem)
library(OpenMx)

AvaluesNested <- matrix(0, 5, 5)
AvaluesNested[2, 1] <- 0.4
AvaluesNested[3, 2] <- 0.4
AvaluesNested[4, 3] <- 0.4
AvaluesNested[5, 4] <- 0.4
AfreeNested <- matrix(FALSE, 5, 5)
AfreeNested[2, 1] <- TRUE
AfreeNested[3, 2] <- TRUE
AfreeNested[4, 3] <- TRUE
AfreeNested[5, 4] <- TRUE
AlabelsNested <- matrix(NA, 5, 5)
AlabelsNested[2, 1] <- "con"
AlabelsNested[3, 2] <- "con"
AlabelsNested[4, 3] <- "con"
AlabelsNested[5, 4] <- "con"

Svalues <- diag(c(1, rep(0.8, 4)))
Sfree <- matrix(FALSE, 5, 5)
diag(Sfree) <- TRUE
Fvalues <- diag(5)

popNested <- mxModel("Constrained simplex structure",
    type="RAM",
    mxMatrix(type="Full", nrow=5, ncol=5, values=AvaluesNested, free=AfreeNested, labels=AlabelsNested, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=5, ncol=5, values=Svalues, free=Sfree, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=5, ncol=5, free=FALSE, values=Fvalues, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=5, values=rep(0, 5), free=rep(TRUE, 5), name="M"),
    mxRAMObjective("A","S","F","M", dimnames=paste0("y", 1:5))
)

AvaluesParent <- matrix(0, 5, 5)
AvaluesParent[2, 1] <- 0.4
AvaluesParent[3, 2] <- 0.5
AvaluesParent[4, 3] <- 0.3
AvaluesParent[5, 4] <- 0.7
AfreeParent <- matrix(FALSE, 5, 5)
AfreeParent[2, 1] <- TRUE
AfreeParent[3, 2] <- TRUE
AfreeParent[4, 3] <- TRUE
AfreeParent[5, 4] <- TRUE

popParent <- mxModel("Unconstrained simplex structure",
    type="RAM",
    mxMatrix(type="Full", nrow=5, ncol=5, values=AvaluesParent, free=AfreeParent, byrow=TRUE, name="A"),
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
