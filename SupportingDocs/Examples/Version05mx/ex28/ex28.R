library(simsem)
library(OpenMx)

Avalues <- matrix(0, 12, 12)
Avalues[1:4, 9] <- 1
Avalues[1:4, 10] <- 0:3
Avalues[5:8, 11] <- 1
Avalues[5:8, 12] <- 0:3
Afree <- matrix(FALSE, 12, 12)

SvaluesA <- diag(c(rep(0.5, 8), 1, 0.25, 1, 0.25))
SvaluesA[9, 10] <- SvaluesA[10, 9] <- 0.2
SvaluesA[11, 12] <- SvaluesA[12, 11] <- 0.2
SvaluesA[9, 11] <- SvaluesA[11, 9] <- 0.5
SfreeA <- matrix(FALSE, 12, 12)
diag(SfreeA) <- TRUE
SfreeA[9, 10] <- SfreeA[10, 9] <- TRUE
SfreeA[11, 12] <- SfreeA[12, 11] <- TRUE
SfreeA[9, 11] <- SfreeA[11, 9] <- TRUE

Fvalues <- cbind(diag(8), matrix(0, 8, 4))

Mvalues <- c(rep(0, 8), 5, 2, 5, 2)
Mfree <- c(rep(FALSE, 8), TRUE, TRUE, TRUE, TRUE)

popA <- mxModel("Model A",
    type="RAM",
    mxMatrix(type="Full", nrow=12, ncol=12, values=Avalues, free=Afree, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=12, ncol=12, values=SvaluesA, free=SfreeA, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=8, ncol=12, free=FALSE, values=Fvalues, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=12, values=Mvalues, free=Mfree, name="M"),
    mxRAMObjective("A","S","F","M", dimnames=c(paste0("x", 1:4), paste0("y", 1:4), "ix", "sx", "iy", "sy"))
)

SvaluesB <- diag(c(rep(0.5, 8), 1, 0.25, 1, 0.25))
SvaluesB[9, 10] <- SvaluesB[10, 9] <- 0.2
SvaluesB[11, 12] <- SvaluesB[12, 11] <- 0.2
SvaluesB[10, 12] <- SvaluesB[12, 10] <- 0.125
SfreeB <- matrix(FALSE, 12, 12)
diag(SfreeB) <- TRUE
SfreeB[9, 10] <- SfreeB[10, 9] <- TRUE
SfreeB[11, 12] <- SfreeB[12, 11] <- TRUE
SfreeB[10, 12] <- SfreeB[12, 10] <- TRUE

popB <- mxModel("Model B",
    type="RAM",
    mxMatrix(type="Full", nrow=12, ncol=12, values=Avalues, free=Afree, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=12, ncol=12, values=SvaluesB, free=SfreeB, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=8, ncol=12, free=FALSE, values=Fvalues, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=12, values=Mvalues, free=Mfree, name="M"),
    mxRAMObjective("A","S","F","M", dimnames=c(paste0("x", 1:4), paste0("y", 1:4), "ix", "sx", "iy", "sy"))
)

outAA <- sim(1000, n = 200, model = popA, generate = popA)
outAB <- sim(1000, n = 200, model = popB, generate = popA)
outBA <- sim(1000, n = 200, model = popA, generate = popB)
outBB <- sim(1000, n = 200, model = popB, generate = popB)

getCutoffNonNested(outAA, outAB, outBA, outBB)
getCutoffNonNested(outAA, outAB)
getCutoffNonNested(outBB, outBA)
getCutoffNonNested(outAA, outAB, outBA, outBB, onetailed=TRUE)
plotCutoffNonNested(outAA, outAB, outBA, outBB, alpha=0.05)
plotCutoffNonNested(outAA, outAB, outBA, outBB, alpha=0.05, onetailed=TRUE)

getPowerFitNonNested(outBA, outBB, dat1Mod1=outAA, dat1Mod2=outAB)
plotPowerFitNonNested(outBA, outBB, dat1Mod1=outAA, dat1Mod2=outAB)
plotPowerFitNonNested(outBA, outBB, dat1Mod1=outAA, dat1Mod2=outAB, usedFit="AIC")

cutoff <- c(AIC=0, BIC=0)
cutoff2 <- c(AIC=2, BIC=2)
getPowerFitNonNested(outBA, outBB, cutoff=cutoff)
getPowerFitNonNested(outBA, outBB, cutoff=cutoff2)
plotPowerFitNonNested(outBA, outBB, cutoff=cutoff2)
plotPowerFitNonNested(outBA, outBB, dat1Mod1=outAA, dat1Mod2=outAB, cutoff=cutoff2)
