library(simsem)
library(lavaan)
library(OpenMx)

AvaluesNested <- matrix(0, 6, 6)
AvaluesNested[1:4, 5] <- 1
AvaluesNested[1:4, 6] <- 0:3
AfreeNested <- matrix(FALSE, 6, 6)

Svalues <- diag(c(rep(1.2, 4), 1, 0.25))
Svalues[5, 6] <- Svalues[6, 5] <- 0.05
Sfree <- matrix(FALSE, 6, 6)
diag(Sfree) <- TRUE
Sfree[5, 6] <- Sfree[6, 5] <- TRUE
Fvalues <- cbind(diag(4), matrix(0, 4, 2))

analyzeNested <- mxModel("Linear Growth",
    type="RAM",
    mxMatrix(type="Full", nrow=6, ncol=6, values=AvaluesNested, free=AfreeNested, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=6, ncol=6, values=Svalues, free=Sfree, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=4, ncol=6, free=FALSE, values=Fvalues, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=6, values=c(rep(0, 4), 5, 2), free=c(rep(FALSE, 4), rep(TRUE, 2)), name="M"),
    mxRAMObjective("A","S","F","M", dimnames=c(paste0("t", 1:4), "i", "s"))
)

AvaluesParent <- matrix(0, 6, 6)
AvaluesParent[1:4, 5] <- 1
AvaluesParent[1:4, 6] <- 0:3
AfreeParent <- matrix(FALSE, 6, 6)
AfreeParent[2:3, 6] <- TRUE

analyzeParent <- mxModel("Unconstrained Growth",
    type="RAM",
    mxMatrix(type="Full", nrow=6, ncol=6, values=AvaluesParent, free=AfreeParent, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=6, ncol=6, values=Svalues, free=Sfree, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=4, ncol=6, free=FALSE, values=Fvalues, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=6, values=c(rep(0, 4), 5, 2), free=c(rep(FALSE, 4), rep(TRUE, 2)), name="M"),
    mxRAMObjective("A","S","F","M", dimnames=c(paste0("t", 1:4), "i", "s"))
)

outNested <- analyze(analyzeNested, data=Demo.growth)
outParent <- analyze(analyzeParent, data=Demo.growth)

samplesize <- nrow(Demo.growth)

simNestedNested <- sim(1000, n = samplesize, model=outNested, generate=outNested, mxFit=TRUE)
simNestedParent <- sim(1000, n = samplesize, model=outParent, generate=outNested, mxFit=TRUE)
simParentNested <- sim(1000, n = samplesize, model=outNested, generate=outParent, mxFit=TRUE)
simParentParent <- sim(1000, n = samplesize, model=outParent, generate=outParent, mxFit=TRUE)

pValueNested(outNested, outParent, simNestedNested, simNestedParent)
getPowerFitNested(simParentNested, simParentParent, nullNested=simNestedNested, nullParent=simNestedParent)
