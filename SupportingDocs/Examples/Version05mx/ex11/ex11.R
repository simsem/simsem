library(simsem)
library(OpenMx)

AvaluesNull <- matrix(0, 7, 7)
AvaluesNull[1:6, 7] <- 0.7
AfreeNull <- matrix(FALSE, 7, 7)
AfreeNull[1:6, 7] <- TRUE

SvaluesNull <- diag(c(rep(0.51, 6), 1))
SfreeNull <- matrix(FALSE, 7, 7)
diag(SfreeNull)[1:6] <- TRUE
FvaluesNull <- cbind(diag(6), matrix(0, 6, 1))

popNull <- mxModel("Null Hypothesis Model",
    type="RAM",
    mxMatrix(type="Full", nrow=7, ncol=7, values=AvaluesNull, free=AfreeNull, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=7, ncol=7, values=SvaluesNull, free=SfreeNull, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=6, ncol=7, free=FALSE, values=FvaluesNull, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=7, values=rep(0, 7), free=c(rep(TRUE, 6), FALSE), name="M"),
    mxRAMObjective("A","S","F","M", dimnames=c(paste0("y", 1:6), "f1"))
)

AvaluesAlt <- matrix(0, 8, 8)
AvaluesAlt[1:3, 7] <- 0.7
AvaluesAlt[4:6, 8] <- 0.7
AfreeAlt <- matrix(FALSE, 8, 8)
AfreeAlt[1:3, 7] <- TRUE
AfreeAlt[4:6, 8] <- TRUE

SvaluesAlt <- diag(c(rep(0.51, 6), 1, 1))
SvaluesAlt[7, 8] <- SvaluesAlt[8, 7] <- 0.5
SfreeAlt <- matrix(FALSE, 8, 8)
diag(SfreeAlt)[1:6] <- TRUE
SfreeAlt[7, 8] <- SfreeAlt[8, 7] <- TRUE
FvaluesAlt <- cbind(diag(6), matrix(0, 6, 2))

popAlt <- mxModel("Alternative Hypothesis Model",
    type="RAM",
    mxMatrix(type="Full", nrow=8, ncol=8, values=AvaluesAlt, free=AfreeAlt, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=8, ncol=8, values=SvaluesAlt, free=SfreeAlt, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=6, ncol=8, free=FALSE, values=FvaluesAlt, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=8, values=rep(0, 8), free=c(rep(TRUE, 6), rep(FALSE, 2)), name="M"),
    mxRAMObjective("A","S","F","M", dimnames=c(paste0("y", 1:6), "f1", "f2"))
)

Output.NULL <- sim(1000, popNull, n=200, generate=popNull, mxFit=TRUE)
getCutoff(Output.NULL, 0.05)
plotCutoff(Output.NULL, 0.05)
summary(Output.NULL)

Output.ALT <- sim(1000, popNull, n=200, generate=popAlt, mxFit=TRUE)
getCutoff(Output.ALT, 0.05)
plotCutoff(Output.ALT, 0.05)
summary(Output.ALT)

cutoff <- getCutoff(Output.NULL, 0.05)
getPowerFit(Output.ALT, cutoff)
plotPowerFit(Output.ALT, Output.NULL, alpha=0.05)
plotPowerFit(Output.ALT, Output.NULL, alpha=0.05, usedFit=c("RMSEA", "SRMR", "CFI"))

cutoff2 <- c(RMSEA = 0.05, CFI = 0.95, TLI = 0.95, SRMR = 0.06)
getPowerFit(Output.ALT, cutoff2)
plotPowerFit(Output.ALT, cutoff=cutoff2)
plotPowerFit(Output.ALT, cutoff=cutoff2, usedFit=c("RMSEA", "SRMR", "CFI"))

plotPowerFit(Output.ALT, Output.NULL, cutoff=cutoff2, usedFit=c("RMSEA", "SRMR", "CFI"))
