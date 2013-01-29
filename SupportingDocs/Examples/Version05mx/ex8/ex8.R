library(simsem)
library(OpenMx)

Avalues <- matrix(0, 13, 13)
Avalues[1:3, 10] <- 0.65
Avalues[4:6, 11] <- 0.65
Avalues[7:9, 12] <- 0.65
Avalues[c(1, 4, 7), 13] <- 0.4
Afree <- matrix(FALSE, 13, 13)
Afree[1:3, 10] <- TRUE
Afree[4:6, 11] <- TRUE
Afree[7:9, 12] <- TRUE
Afree[c(1, 4, 7), 13] <- TRUE

Svalues <- diag(c(rep(0.5, 9), rep(1, 4)))
Svalues[10, 11] <- Svalues[11, 10] <- 0.4
Svalues[10, 12] <- Svalues[12, 10] <- 0.2
Svalues[11, 12] <- Svalues[12, 11] <- 0.3
Sfree <- matrix(FALSE, 13, 13)
diag(Sfree)[1:9] <- TRUE
Sfree[10, 11] <- Sfree[11, 10] <- TRUE
Sfree[10, 12] <- Sfree[12, 10] <- TRUE
Sfree[11, 12] <- Sfree[12, 11] <- TRUE
Fvalues <- cbind(diag(9), matrix(0, 9, 4))

popModel <- mxModel("Three Factor Model with method factor",
    type="RAM",
    mxMatrix(type="Full", nrow=13, ncol=13, values=Avalues, free=Afree, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=13, ncol=13, values=Svalues, free=Sfree, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=9, ncol=13, free=FALSE, values=Fvalues, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=13, values=rep(0, 13), free=c(rep(TRUE, 9), rep(FALSE, 4)), name="M"),
    mxRAMObjective("A","S","F","M", dimnames=c(paste0("y", 1:9), "f1", "f2", "f3", "f4"))
)

miss.model <- miss(pmMCAR=0.2) # Multiple-imputation is not available in OpenMx currently 

Output <- sim(1000, n=500, model=popModel, miss=miss.model)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)
