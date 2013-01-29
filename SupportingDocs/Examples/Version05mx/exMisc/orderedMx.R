library(simsem)
library(OpenMx)

Avalues <- matrix(0, 5, 5)
Avalues[1:4, 5] <- 1
Afree <- matrix(FALSE, 5, 5)
Afree[1:4, 5] <- TRUE

Svalues <- diag(rep(1, 5))
Sfree <- matrix(FALSE, 5, 5) # Fix theta = 1
Fvalues <- cbind(diag(4), matrix(0, 4, 1))

popModel <- mxModel("Ordinal Factor Analysis",
    type="RAM",
    mxMatrix(type="Full", nrow=5, ncol=5, values=Avalues, free=Afree, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=5, ncol=5, values=Svalues, free=Sfree, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=4, ncol=5, free=FALSE, values=Fvalues, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=5, values=rep(0, 5), free=rep(FALSE, 5), name="M"),
	mxMatrix(type="Full", nrow=1, ncol=4, free=rep(TRUE, 4), values=c(0.5, 0.25, 0, -0.5), byrow=TRUE, name="thresh"),
    mxRAMObjective("A","S","F","M", dimnames=c(paste0("y", 1:4), "f1"), thresholds = "thresh", threshnames=paste0("y", 1:4))
)

Output <- sim(1000, n = 1000, model = popModel, mxFit=TRUE)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)

Output2 <- sim(NULL, n = 50:500, model = popModel, mxFit=TRUE)
getCutoff(Output2, 0.05, nVal = 250)
plotCutoff(Output2, 0.05)
summary(Output2)
