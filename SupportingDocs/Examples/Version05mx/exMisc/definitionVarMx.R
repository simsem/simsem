library(simsem)
library(OpenMx)

# Create Timepoints of each individual
samplesize <- 200
timeMeasured <- matrix(NA, samplesize, 4)
timeFrame <- 0:8
for(i in 1:samplesize) {
	timeMeasured[i,] <- sort(sample(timeFrame, 4))
}
colnames(timeMeasured) <- paste0("t", 1:4)

Avalues <- matrix(0, 6, 6)
Avalues[1:4, 5] <- 1
Afree <- matrix(FALSE, 6, 6)
# Include labels for definition variable
Alabels <- matrix(NA, 6, 6)
Alabels[1:4, 6] <- paste0("data.t", 1:4)

Svalues <- diag(c(rep(1, 4), 1, 0.25))
Svalues[5, 6] <- Svalues[6, 5] <- -0.2
Sfree <- matrix(FALSE, 6, 6) 
diag(Sfree) <- TRUE
Sfree[5, 6] <- Sfree[6, 5] <- TRUE

Fvalues <- cbind(diag(4), matrix(0, 4, 2))

Mvalues <- c(rep(0, 4), 5, 2)
Mfree <- c(rep(FALSE, 4), TRUE, TRUE)

popModel <- mxModel("Growth Model with Varying Time Points",
    type="RAM",
    mxMatrix(type="Full", nrow=6, ncol=6, values=Avalues, free=Afree, labels=Alabels, byrow=TRUE, name="A"),
    mxMatrix(type="Symm", nrow=6, ncol=6, values=Svalues, free=Sfree, byrow=TRUE, name="S"),
    mxMatrix(type="Full", nrow=4, ncol=6, free=FALSE, values=Fvalues, byrow=TRUE, name="F"),
    mxMatrix(type="Full", nrow=1, ncol=6, values=Mvalues, free=Mfree, name="M"),
    mxExpectationRAM("A","S","F","M", dimnames=c(paste0("y", 1:4), "i", "s"))
)

# Put dataset containing definition variables in the covData argument
dat <- generate(popModel, n = samplesize, covData = timeMeasured)
out <- analyze(popModel, dat)

Output <- sim(1000, n = samplesize, model = popModel, covData = timeMeasured)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)
