library(simsem)
library(OpenMx)

Avalues <- matrix(0, 9, 9)
Avalues[1:3, 8] <- 0.7
Avalues[4:6, 9] <- 0.7
Afree <- matrix(FALSE, 9, 9)
Afree[1:3, 8] <- TRUE
Afree[4:6, 9] <- TRUE

Svalues <- diag(c(rep(0.51, 6), rep(1, 3)))
Svalues[1:6, 7] <- Svalues[7, 1:6] <- 0.4
Svalues[8, 9] <- Svalues[9, 8] <- 0.5
Sfree <- matrix(FALSE, 9, 9)
diag(Sfree)[1:7] <- TRUE
Sfree[1:6, 7] <- Sfree[7, 1:6] <- TRUE
Sfree[8, 9] <- Sfree[9, 8] <- TRUE
Fvalues <- cbind(diag(7), matrix(0, 7, 2))

popModel <- mxModel("Model with auxiliary variable",
    type="RAM",
    mxMatrix(type="Full", nrow=9, ncol=9, values=Avalues, free=Afree, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=9, ncol=9, values=Svalues, free=Sfree, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=7, ncol=9, free=FALSE, values=Fvalues, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=9, values=rep(0, 9), free=c(rep(TRUE, 7), rep(FALSE, 2)), name="M"),
    mxExpectationRAM("A","S","F","M", dimnames=c(paste0("y", 1:7), "f1", "f2"))
)

AvaluesA <- matrix(0, 8, 8)
AvaluesA[1:3, 7] <- 0.7
AvaluesA[4:6, 8] <- 0.7
AfreeA <- matrix(FALSE, 8, 8)
AfreeA[1:3, 7] <- TRUE
AfreeA[4:6, 8] <- TRUE

SvaluesA <- diag(c(rep(0.51, 6), 1, 1))
SvaluesA[7, 8] <- SvaluesA[8, 7] <- 0.5
SfreeA <- matrix(FALSE, 8, 8)
diag(SfreeA)[1:6] <- TRUE
SfreeA[7, 8] <- SfreeA[8, 7] <- TRUE
FvaluesA <- cbind(diag(6), matrix(0, 6, 2))

analyzeModel <- mxModel("Analysis ignore missing",
    type="RAM",
    mxMatrix(type="Full", nrow=8, ncol=8, values=AvaluesA, free=AfreeA, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=8, ncol=8, values=SvaluesA, free=SfreeA, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=6, ncol=8, free=FALSE, values=FvaluesA, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=8, values=rep(0, 8), free=c(rep(TRUE, 6), rep(FALSE, 2)), name="M"),
    mxExpectationRAM("A","S","F","M", dimnames=c(paste0("y", 1:6), "f1", "f2"))
)


missmodel <- miss(pmMAR=0.1, cov="y7", threshold = 0.5) # Auxiliary variable cannot be taken to account automatically when OpenMx is used.

Output <- sim(1000, analyzeModel, n=200, generate=popModel, miss = missmodel)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)
