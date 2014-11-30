library(simsem)
library(lavaan)
library(OpenMx)

Avalues <- matrix(0, 12, 12)
Avalues[1:3, 10] <- c(1, 0.6, 0.7)
Avalues[4:6, 11] <- c(1, 1.1, 0.9)
Avalues[7:9, 12] <- c(1, 1.2, 1.1)
Afree <- matrix(FALSE, 12, 12)
Afree[1:3, 10] <- c(FALSE, TRUE, TRUE)
Afree[4:6, 11] <- c(FALSE, TRUE, TRUE)
Afree[7:9, 12] <- c(FALSE, TRUE, TRUE)

Svalues <- diag(c(0.5, 1.1, 0.8, 0.4, 0.4, 0.8, 0.8, 0.5, 0.6, 0.8, 0.9, 0.4))
Svalues[10, 11] <- Svalues[11, 10] <- 0.4
Svalues[10, 12] <- Svalues[12, 10] <- 0.2
Svalues[11, 12] <- Svalues[12, 11] <- 0.3
Sfree <- matrix(FALSE, 12, 12)
diag(Sfree) <- TRUE
Sfree[10, 11] <- Sfree[11, 10] <- TRUE
Sfree[10, 12] <- Sfree[12, 10] <- TRUE
Sfree[11, 12] <- Sfree[12, 11] <- TRUE
Fvalues <- cbind(diag(9), matrix(0, 9, 3))

popModel <- mxModel("Three Factor Model",
    type="RAM",
    mxMatrix(type="Full", nrow=12, ncol=12, values=Avalues, free=Afree, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=12, ncol=12, values=Svalues, free=Sfree, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=9, ncol=12, free=FALSE, values=Fvalues, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=12, values=rep(0, 12), free=c(rep(TRUE, 9), rep(FALSE, 3)), name="M"),
    mxExpectationRAM("A","S","F","M", dimnames=c(paste0("x", 1:9), "f1", "f2", "f3"))
)

fit <- analyze(popModel, data=HolzingerSwineford1939)

output.nomis <- sim(1000, n = nrow(HolzingerSwineford1939), model = fit, mxFit = TRUE)
plotCutoff(output.nomis, 0.05)
pValue(fit, output.nomis)
