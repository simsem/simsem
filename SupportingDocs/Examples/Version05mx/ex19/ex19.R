library(simsem)
library(OpenMx)

Avalues <- matrix(0, 7, 7)
Avalues[1:4, 6] <- 1
Avalues[1:4, 7] <- 0:3
Avalues[6:7, 5] <- c(0.5, 0.1)
Afree <- matrix(FALSE, 7, 7)
Afree[6:7, 5] <- TRUE

Svalues <- diag(c(rep(1.2, 4), 0.25, 1, 0.25))
Svalues[6, 7] <- Svalues[7, 6] <- 0.05
Sfree <- matrix(FALSE, 7, 7)
diag(Sfree) <- TRUE
Sfree[6, 7] <- Sfree[7, 6] <- TRUE
Fvalues <- cbind(diag(5), matrix(0, 5, 2))

popModel <- mxModel("Two_factor Model",
    type="RAM",
    mxMatrix(type="Full", nrow=7, ncol=7, values=Avalues, free=Afree, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=7, ncol=7, values=Svalues, free=Sfree, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=5, ncol=7, free=FALSE, values=Fvalues, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=7, values=c(rep(0, 4), 0.5, 5, 2), free=c(rep(FALSE, 4), rep(TRUE, 3)), name="M"),
    mxExpectationRAM("A","S","F","M", dimnames=c(paste0("t", 1:4), "x", "i", "s"))
)

pois <- list(lambda = 3)
n01 <- list(mean=0, sd=1)
indDist <- bindDist(c("norm", "norm", "norm", "norm", "pois"), n01, n01, n01, n01, pois, keepScale=c(TRUE, TRUE, TRUE, TRUE, FALSE))

Output <- sim(NULL, n=50:1000, pmMCAR=seq(0, 0.4, 0.1), indDist=indDist, model = popModel)
plotCutoff(Output, 0.05)
getCutoff(Output, 0.05, nVal = 200, pmMCARval = 0)
getCutoff(Output, 0.05, nVal = 300, pmMCARval = 0.33)	

Cpow <- getPower(Output)
Cpow2 <- getPower(Output, nVal = 200, pmMCARval = 0.35)
findPower(Cpow, "N", 0.80)
findPower(Cpow, "MCAR", 0.80)
plotPower(Output, powerParam=c("A[6,5]", "A[7,5]"))
