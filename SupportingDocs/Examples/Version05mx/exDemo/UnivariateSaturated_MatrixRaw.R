library(simsem)
library(semTools)
library(OpenMx)

################### Fitting univSatFit4

set.seed(100)
x <- rnorm (1000, 0, 1)
testData <- as.matrix(x)
selVars <- c("X")
dimnames(testData) <- list(NULL, selVars)
summary(testData)
colMeans(testData)
var(testData)

univSatModel4 <- mxModel("univSat4",
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
        observed=testData, 
        type="raw", 
    ),
    mxExpectationNormal(
        covariance="expCov", 
        means="expMean",
        dimnames=selVars
    ),
	mxFitFunctionML()
)

univSatFit4 <- mxRun(univSatModel4)
fitMeasuresMx(univSatFit4)
univSatFit4Sim <- sim(10, univSatFit4, n = 200)
summary(univSatFit4Sim)
