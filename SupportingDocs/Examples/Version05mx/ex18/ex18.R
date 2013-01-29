library(simsem)

Avalues <- matrix(0, 8, 8)
Avalues[1:3, 7] <- 0.7
Avalues[4:6, 8] <- 0.7
Afree <- matrix(FALSE, 8, 8)
Afree[1:3, 7] <- TRUE
Afree[4:6, 8] <- TRUE

Svalues <- diag(c(rep(0.51, 6), 1, 1))
Svalues[7, 8] <- Svalues[8, 7] <- 0.5
Sfree <- matrix(FALSE, 8, 8)
diag(Sfree)[1:6] <- TRUE
Sfree[7, 8] <- Sfree[8, 7] <- TRUE
Fvalues <- cbind(diag(6), matrix(0, 6, 2))

popModel <- mxModel("Two_factor Model",
    type="RAM",
    mxMatrix(type="Full", nrow=8, ncol=8, values=Avalues, free=Afree, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=8, ncol=8, values=Svalues, free=Sfree, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=6, ncol=8, free=FALSE, values=Fvalues, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=8, values=rep(0, 8), free=c(rep(TRUE, 6), rep(FALSE, 2)), name="M"),
    mxRAMObjective("A","S","F","M", dimnames=c(paste0("y", 1:6), "f1", "f2"))
)

Output <- sim(NULL, n=50:1000, model = popModel)
summary(Output)
plotCutoff(Output, 0.05)
getCutoff(Output, 0.05, nVal = 200)	

Cpow <- getPower(Output)
Cpow2 <- getPower(Output, nVal = 200)
findPower(Cpow, "N", 0.80)
plotPower(Output, powerParam=c("A[1,7]", "S[7,8]"))
