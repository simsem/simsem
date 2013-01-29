library(simsem)
library(lavaan)
library(OpenMx)

Avalues <- matrix(0, 14, 14)
Avalues[1:3, 12] <- 1
Avalues[4:7, 13] <- 1
Avalues[8:11, 14] <- 1
Avalues[13, 12] <- 0.5
Avalues[14, 12:13] <- 0.5
Afree <- matrix(FALSE, 14, 14)
Afree[2:3, 12] <- TRUE
Afree[5:7, 13] <- TRUE
Afree[9:11, 14] <- TRUE
Afree[13, 12] <- TRUE
Afree[14, 12:13] <- TRUE
Alabels <- matrix(NA, 14, 14)
Alabels[5:7, 13] <- c("a", "b", "c")
Alabels[9:11, 14] <- c("a", "b", "c")

Svalues <- diag(c(rep(0.5, 11), 1, 1, 1))
Svalues[4, 8] <- Svalues[8, 4] <- 0.2
Svalues[5, 7] <- Svalues[7, 5] <- 0.2
Svalues[5, 9] <- Svalues[9, 5] <- 0.2
Svalues[6, 10] <- Svalues[10, 6] <- 0.2
Svalues[7, 11] <- Svalues[11, 7] <- 0.2
Svalues[9, 11] <- Svalues[11, 9] <- 0.2
Sfree <- matrix(FALSE, 14, 14)
diag(Sfree) <- TRUE
Sfree[4, 8] <- Sfree[8, 4] <- TRUE
Sfree[5, 7] <- Sfree[7, 5] <- TRUE
Sfree[5, 9] <- Sfree[9, 5] <- TRUE
Sfree[6, 10] <- Sfree[10, 6] <- TRUE
Sfree[7, 11] <- Sfree[11, 7] <- TRUE
Sfree[9, 11] <- Sfree[11, 9] <- TRUE
Fvalues <- cbind(diag(11), matrix(0, 11, 3))

analyzeModel <- mxModel("PoliticalDemocracy Data",
    type="RAM",
    mxMatrix(type="Full", nrow=14, ncol=14, values=Avalues, free=Afree, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=14, ncol=14, values=Svalues, free=Sfree, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=11, ncol=14, free=FALSE, values=Fvalues, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=14, values=rep(0, 14), free=c(rep(TRUE, 11), rep(FALSE, 3)), name="M"),
    mxRAMObjective("A","S","F","M", dimnames=c(paste0("x", 1:3), paste0("y", 1:8), "ind60", "dem60", "dem65"))
)

usedData <- imposeMissing(PoliticalDemocracy, pmMCAR=0.03)
fit <- analyze(analyzeModel, data=usedData)

misstemplate <- miss(logical=is.na(usedData))
Output <- sim(1000, n=nrow(usedData), model=fit, miss=misstemplate, mxFit=TRUE)
pValue(fit, Output)
