library(simsem)
library(OpenMx)

Avalues <- matrix(0, 11, 11)
Avalues[1:3, 9] <- 0.7
Avalues[4:6, 10] <- 0.7
Avalues[7:8, 11] <- 0.6
Avalues[11, 9] <- 0.6
Avalues[11, 10] <- 0.3
Afree <- matrix(FALSE, 11, 11)
Afree[1:3, 9] <- TRUE
Afree[4:6, 10] <- TRUE
Afree[7:8, 11] <- TRUE
Afree[11, 9] <- TRUE
Afree[11, 10] <- TRUE
Alabels <- matrix(NA, 11, 11)
Alabels[7:8, 11] <- "con1"

Svalues <- diag(c(rep(0.51, 6), rep(0.64, 2), rep(1, 3)))
Svalues[9, 10] <- Svalues[10, 9] <- 0.5
Sfree <- matrix(FALSE, 11, 11)
diag(Sfree)[1:8] <- TRUE
Sfree[9, 10] <- Sfree[10, 9] <- TRUE
Fvalues <- cbind(diag(8), matrix(0, 8, 3))

popModel <- mxModel("Latent Regression Model",
    type="RAM",
    mxMatrix(type="Full", nrow=11, ncol=11, values=Avalues, free=Afree, labels=Alabels, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=11, ncol=11, values=Svalues, free=Sfree, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=8, ncol=11, free=FALSE, values=Fvalues, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=11, values=rep(0, 11), free=c(rep(TRUE, 8), rep(FALSE, 3)), name="M"),
    mxRAMObjective("A","S","F","M", dimnames=c(paste0("y", 1:8), "f1", "f2", "f3"))
)

Output <- sim(1000, popModel, n=200)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)
