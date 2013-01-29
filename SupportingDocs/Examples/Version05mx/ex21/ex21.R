library(simsem)
library(OpenMx)

AvaluesNull <- matrix(0, 10, 10)
AvaluesNull[1:5, 9] <- 0.7
AvaluesNull[6:8, 10] <- 0.7
AfreeNull <- matrix(FALSE, 10, 10)
AfreeNull[1:5, 9] <- TRUE
AfreeNull[6:8, 10] <- TRUE

Svalues <- diag(c(rep(0.51, 8), 1, 1))
Svalues[9, 10] <- Svalues[10, 9] <- 0.5
Sfree <- matrix(FALSE, 10, 10)
diag(Sfree)[1:8] <- TRUE
Sfree[9, 10] <- Sfree[10, 9] <- TRUE
Fvalues <- cbind(diag(8), matrix(0, 8, 2))

popNull <- mxModel("Null Hypothesis Model",
    type="RAM",
    mxMatrix(type="Full", nrow=10, ncol=10, values=AvaluesNull, free=AfreeNull, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=10, ncol=10, values=Svalues, free=Sfree, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=8, ncol=10, free=FALSE, values=Fvalues, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=10, values=rep(0, 10), free=c(rep(TRUE, 8), rep(FALSE, 4)), name="M"),
    mxRAMObjective("A","S","F","M", dimnames=c(paste0("y", 1:8), "f1", "f2"))
)

AvaluesAlt <- matrix(0, 10, 10)
AvaluesAlt[1:4, 9] <- 0.7
AvaluesAlt[5:8, 10] <- 0.7
AfreeAlt <- matrix(FALSE, 10, 10)
AfreeAlt[1:4, 9] <- TRUE
AfreeAlt[5:8, 10] <- TRUE

popAlt <- mxModel("Alternative Hypothesis Model",
    type="RAM",
    mxMatrix(type="Full", nrow=10, ncol=10, values=AvaluesAlt, free=AfreeAlt, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=10, ncol=10, values=Svalues, free=Sfree, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=8, ncol=10, free=FALSE, values=Fvalues, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=10, values=rep(0, 10), free=c(rep(TRUE, 8), rep(FALSE, 4)), name="M"),
    mxRAMObjective("A","S","F","M", dimnames=c(paste0("y", 1:8), "f1", "f2"))
)

Output.NULL <- sim(NULL, n = 25:500, popNull, generate = popNull, mxFit=TRUE)
Output.ALT <- sim(NULL, n = 25:500, popNull, generate = popAlt, mxFit=TRUE)

cutoff <- getCutoff(Output.NULL, alpha = 0.05, nVal = 250)
plotCutoff(Output.NULL, alpha = 0.05)
getPowerFit(Output.ALT, nullObject = Output.NULL, alpha = 0.05, nVal = 250)
getPowerFit(Output.ALT, cutoff = cutoff, nVal = 250, condCutoff = TRUE)
plotPowerFit(Output.ALT, Output.NULL, alpha = 0.05)
plotPowerFit(Output.ALT, Output.NULL, alpha = 0.05, logistic = FALSE)

cutoff2 <- c(RMSEA = 0.05, CFI = 0.95, TLI = 0.95, SRMR = 0.06)
getPowerFit(Output.ALT, cutoff = cutoff2, nVal = 250, condCutoff = FALSE)
plotPowerFit(Output.ALT, cutoff = cutoff2)
plotPowerFit(Output.ALT, cutoff = cutoff2, logistic = FALSE)
