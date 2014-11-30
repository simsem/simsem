library(simsem)
library(OpenMx)

Avalues <- matrix(0, 15, 15)
Avalues[1:4, 13] <- 0.7
Avalues[5:8, 14] <- 0.7
Avalues[9:12, 15] <- 0.7
Afree <- matrix(FALSE, 15, 15)
Afree[1:4, 13] <- FALSE
Afree[5:8, 14] <- FALSE
Afree[9:12, 15] <- FALSE

Svalues <- diag(c(rep(0.51, 12), rep(1, 3)))
Svalues[13, 14] <- Svalues[14, 13] <- 0.5
Svalues[13, 15] <- Svalues[15, 13] <- 0.5
Svalues[14, 15] <- Svalues[15, 14] <- 0.5
Sfree <- matrix(FALSE, 15, 15)
diag(Sfree)[1:12] <- TRUE
Sfree[13, 14] <- Sfree[14, 13] <- TRUE
Sfree[13, 15] <- Sfree[15, 13] <- TRUE
Sfree[14, 15] <- Sfree[15, 14] <- TRUE
Fvalues <- cbind(diag(12), matrix(0, 12, 3))

popModel <- mxModel("Model with auxiliary variable",
    type="RAM",
    mxMatrix(type="Full", nrow=15, ncol=15, values=Avalues, free=Afree, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=15, ncol=15, values=Svalues, free=Sfree, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=12, ncol=15, free=FALSE, values=Fvalues, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=15, values=rep(0, 15), free=c(rep(TRUE, 12), rep(FALSE, 3)), name="M"),
    mxExpectationRAM("A","S","F","M", dimnames=c(paste0("y", 1:12), "f1", "f2", "f3"))
)

distname <- c(rep("t", 4), rep("chisq", 8))

d1 <- list(df=2)
d2 <- list(df=3)
d3 <- list(df=4)
d4 <- list(df=5)
d5 <- list(df=3)
d6 <- list(df=4)
d7 <- list(df=5)
d8 <- list(df=6)
d9 <- list(df=3)
d10 <- list(df=4)
d11 <- list(df=5)
d12 <- list(df=6)

dist <- bindDist(distname, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, reverse=c(rep(FALSE, 8), rep(TRUE, 4)))

Output <- sim(1000, n=200, popModel, indDist=dist)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)

dist2 <- bindDist(skewness = seq(-5, 5, length.out=12), kurtosis = seq(2, 10, length.out=12))

Output2 <- sim(1000, n=200, popModel, indDist=dist2)
getCutoff(Output2, 0.05)
plotCutoff(Output2, 0.05)
summary(Output2)
