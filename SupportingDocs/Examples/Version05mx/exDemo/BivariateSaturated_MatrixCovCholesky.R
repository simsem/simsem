library(simsem)
library(semTools)
library(OpenMx)
library(MASS)

################### Fitting bivSatModel5m

# The bivSatModel5 does not work with simsem because the expected mean is not specified.

set.seed(200)
rs=.5
xy <- mvrnorm (1000, c(0,0), matrix(c(1,rs,rs,1),2,2))
testData <- xy
selVars <- c("X","Y")
dimnames(testData) <- list(NULL, selVars)
summary(testData)
cov(testData)

bivSatModel5m <- mxModel("bivSat5m",
    mxMatrix(
        type="Lower", 
        nrow=2, 
        ncol=2, 
        free=T, 
        values=.5, 
        name="Chol"
    ),
    mxAlgebra(
        expression=Chol %*% t(Chol), 
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

bivSatFit5m <- mxRun(bivSatModel5m)
fitMeasuresMx(bivSatFit5m)
bivSatFit5mSim <- sim(10, bivSatFit5m, n = 200)
summary(bivSatFit5mSim)
