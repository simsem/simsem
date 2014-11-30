library(simsem)
library(OpenMx)

Avalues <- matrix(0, 8, 8)
Avalues[1:3, 6] <- 0.8
Avalues[4, 7] <- 1
Avalues[5, 8] <- 1
Avalues[8, 6] <- 0.5
Avalues[8, 7] <- 0.5
Afree <- matrix(FALSE, 8, 8)
Afree[1:3, 6] <- TRUE
Afree[8, 6] <- TRUE
Afree[8, 7] <- TRUE

Svalues <- diag(c(rep(0.36, 3), 0, 0, 1, 1, 0.5))
Svalues[6, 7] <- Svalues[7, 6] <- 0.2
Sfree <- matrix(FALSE, 8, 8)
diag(Sfree)[1:3] <- TRUE
Sfree[6, 7] <- Sfree[7, 6] <- TRUE
Fvalues <- cbind(diag(5), matrix(0, 5, 3))

popModel <- mxModel("Model with Single Indicators",
    type="RAM",
    mxMatrix(type="Full", nrow=8, ncol=8, values=Avalues, free=Afree, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=8, ncol=8, values=Svalues, free=Sfree, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=5, ncol=8, free=FALSE, values=Fvalues, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=8, values=rep(0, 8), free=c(rep(TRUE, 5), rep(FALSE, 3)), name="M"),
    mxExpectationRAM("A","S","F","M", dimnames=c(paste0("y", 1:5), "f1", "f2", "f3"))
)

dist <- c("norm", "norm", "norm", "chisq", "norm")
n01 <- list(mean=0, sd=1)
c5 <- list(df=5)
facDist <- bindDist(dist, n01, n01, n01, c5, n01)

Output <- sim(1000, n=200, popModel, indDist=facDist)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)
