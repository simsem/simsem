library(simsem)
library(semTools)
library(OpenMx)

################### Fitting univSatFit3m

# Note that the univSatFit3 model does not work with simsem. The simsem package use raw data to fit a model so the expected mean must be also specified.

set.seed(100)
x <- rnorm (1000, 0, 1)
testData <- as.matrix(x)
selVars <- c("X")
dimnames(testData) <- list(NULL, selVars)
summary(testData)
colMeans(testData)
var(testData)

univSatModel3m <- mxModel("univSat3m",
    mxMatrix(
        type="Symm", 
        nrow=1, 
        ncol=1, 
        free=T, 
        values=1, 
        name="expCov"
    ),
    mxMatrix(
        type="Full", 
        nrow=1, 
        ncol=1, 
        free=T, 
        values=0, 
        name="expMean"
    ),
    mxData(
        observed=var(testData), 
        type="cov", 
        numObs=1000,
        means=colMeans(testData) 
    ),
    mxExpectationNormal(
        covariance="expCov", 
        means="expMean",
        dimnames=selVars
    )
)

univSatFit3m <- mxRun(univSatModel3m)
fitMeasuresMx(univSatFit3m)
univSatFit3mSim <- sim(10, univSatFit3m, n = 200)
summary(univSatFit3mSim)

