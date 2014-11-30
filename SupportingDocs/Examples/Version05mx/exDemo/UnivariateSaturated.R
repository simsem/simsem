library(simsem)
library(semTools)
library(OpenMx)

################### Fitting univSatFit1m

# Note that the univSatFit1 model does not work with simsem. The simsem package use raw data to fit a model so the expected mean must be also specified.

set.seed(100)
x <- rnorm (1000, 0, 1)
testData <- as.matrix(x)
selVars <- c("X")
dimnames(testData) <- list(NULL, selVars)
summary(testData)
colMeans(testData)
var(testData)

univSatModel1m <- mxModel("univSat1m",
    manifestVars=selVars,
    mxPath(
        from=c("X"), 
        arrows=2, 
        free=T, 
        values=1, 
        lbound=.01, 
        labels="vX"
    ),
    mxPath(
        from="one", 
        to="X", 
        arrows=1, 
        free=T, 
        values=0, 
        labels="mX"
    ),
    mxData(
        observed=var(testData), 
        type="cov", 
        numObs=1000, 
        means=colMeans(testData)
    ),
    type="RAM"
)
univSatFit1m <- mxRun(univSatModel1m)
fitMeasuresMx(univSatFit1m)
univSatFit1mSim <- sim(10, univSatFit1m, n = 200)
summary(univSatFit1mSim)

################### Fitting univSatFit2

univSatModel2 <- mxModel("univSat2",
    manifestVars= selVars,
    mxPath(
        from=c("X"), 
        arrows=2, 
        free=T, 
        values=1, 
        lbound=.01, 
        labels="vX"
    ),
    mxPath(
        from="one", 
        to="X", 
        arrows=1, 
        free=T, 
        values=0, 
        labels="mX"
    ),    
    mxData(
        observed=testData, 
        type="raw", 
    ),
    type="RAM"
)
univSatFit2 <- mxRun(univSatModel2)
fitMeasuresMx(univSatFit2)
univSatFit2Sim <- sim(10, univSatFit2, n = 200)
summary(univSatFit2Sim)

################### Fitting univSatFit2s

univSatModel2s <- mxModel(univSatModel1m,
    mxData(
        observed=testData, 
        type="raw"
    ),
    mxPath(
        from="one", 
        to="X", 
        arrows=1, 
        free=T, 
        values=0, 
        labels="mX"
    ),    
    name="univSat2s",
    type="RAM"
    )
univSatFit2s <- mxRun(univSatModel2s)
fitMeasuresMx(univSatFit2s)
univSatFit2sSim <- sim(10, univSatFit2s, n = 200)
summary(univSatFit2sSim)

################### Fitting univSatFit3m

# Note that the univSatFit3 model does not work with simsem. The simsem package use raw data to fit a model so the expected mean must be also specified.

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

################### Fitting univSatFit4

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
    )
)
univSatFit4 <- mxRun(univSatModel4)
fitMeasuresMx(univSatFit4)
univSatFit4Sim <- sim(10, univSatFit4, n = 200)
summary(univSatFit4Sim)
