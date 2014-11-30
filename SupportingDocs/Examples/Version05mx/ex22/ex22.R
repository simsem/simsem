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

SvaluesNull <- diag(c(1, rep(0.64, 4)))
SfreeNull <- matrix(FALSE, 5, 5)
diag(SfreeNull) <- TRUE
FvaluesNull <- diag(5)

popNull <- mxModel("Null Hypothesis Model",
    type="RAM",
    mxMatrix(type="Full", nrow=5, ncol=5, values=AvaluesNull, free=AfreeNull, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=5, ncol=5, values=SvaluesNull, free=SfreeNull, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=5, ncol=5, free=FALSE, values=FvaluesNull, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=5, values=rep(0, 5), free=rep(TRUE, 5), name="M"),
    mxExpectationRAM("A","S","F","M", dimnames=paste0("y", 1:5))
)

AvaluesAlt <- matrix(0, 5, 5)
AvaluesAlt[2, 1] <- 0.4
AvaluesAlt[3, 1] <- 0.4
AvaluesAlt[4, 2:3] <- 0.4
AvaluesAlt[5, 4] <- 0.4
AfreeAlt <- matrix(FALSE, 5, 5)
AfreeAlt[2, 1] <- TRUE
AfreeAlt[3, 1] <- TRUE
AfreeAlt[4, 2:3] <- TRUE
AfreeAlt[5, 4] <- TRUE

SvaluesAlt <- diag(c(1, 0.64, 0.64, 0.40, 0.64))
SfreeAlt <- matrix(FALSE, 5, 5)
diag(SfreeAlt) <- TRUE
FvaluesAlt <- diag(5)

popAlt <- mxModel("Alternative Hypothesis Model",
    type="RAM",
    mxMatrix(type="Full", nrow=5, ncol=5, values=AvaluesAlt, free=AfreeAlt, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=5, ncol=5, values=SvaluesAlt, free=SfreeAlt, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=5, ncol=5, free=FALSE, values=FvaluesAlt, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=5, values=rep(0, 5), free=rep(TRUE, 5), name="M"),
    mxExpectationRAM("A","S","F","M", dimnames=paste0("y", 1:5))
)

Output.NULL <- sim(NULL, n = 25:500, popNull, generate = popNull, pmMCAR = seq(0, 0.3, 0.1), mxFit=TRUE)
Output.ALT <- sim(NULL, n = 25:500, popNull, generate = popAlt, pmMCAR = seq(0, 0.3, 0.1), mxFit=TRUE)

cutoff <- getCutoff(Output.NULL, alpha = 0.05, nVal = 250, pmMCARval = 0.2)
plotCutoff(Output.NULL, alpha = 0.05)
getPowerFit(Output.ALT, nullObject = Output.NULL, alpha = 0.05, nVal = 250, pmMCARval = 0.2)
getPowerFit(Output.ALT, cutoff = cutoff, nVal = 250, pmMCARval = 0.2, condCutoff = TRUE)
plotPowerFit(Output.ALT, Output.NULL, alpha = 0.05)

cutoff2 <- c(RMSEA = 0.05, CFI = 0.95, TLI = 0.95, SRMR = 0.06)
getPowerFit(Output.ALT, cutoff = cutoff2, nVal = 250, pmMCARval = 0.2, condCutoff = FALSE)
plotPowerFit(Output.ALT, cutoff = cutoff2)
