library(simsem)
library(semTools)
library(OpenMx)
library(MASS)

########################### Fitting bivSatFit3

# The bivSatFit3 is not applicable in the simsem since the expected means are not specified.

set.seed(200)
rs=.5
xy <- mvrnorm (1000, c(0,0), matrix(c(1,rs,rs,1),2,2))
testData <- xy
selVars <- c("X","Y")
dimnames(testData) <- list(NULL, selVars)
summary(testData)
covData <- cov(testData)

bivSatModel3m <- mxModel("bivSat3m",
    mxMatrix(
        type="Symm", 
        nrow=2, 
        ncol=2, 
        free=T, 
        values=c(1,.5,1), 
        name="expCov"
    ),
    mxMatrix(
        type="Full", 
        nrow=1, 
        ncol=2, 
        free=T, 
        values=c(0,0), 
        name="expMean"
    ),
    mxData(
        observed=cov(testData), 
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

bivSatFit3m <- mxRun(bivSatModel3m)
fitMeasuresMx(bivSatFit3m)
bivSatFit3mSim <- sim(10, bivSatFit3m, n = 200)
summary(bivSatFit3mSim)
