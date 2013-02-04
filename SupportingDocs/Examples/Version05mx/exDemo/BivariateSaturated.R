library(simsem)
library(semTools)
library(OpenMx)
library(MASS)

########################## Fitting bivSatFit1m

# The bivSatFit1 object does not work with simsem because the expected mean is not specified.

set.seed(200)
rs=.5
xy <- mvrnorm (1000, c(0,0), matrix(c(1,rs,rs,1),2,2))
testData <- xy
selVars <- c('X','Y')
dimnames(testData) <- list(NULL, selVars)
summary(testData)
cov(testData)

bivSatModel1m <- mxModel("bivSat1m",
    manifestVars= selVars,
    mxPath(
        from=c("X", "Y"), 
        arrows=2, 
        free=T, 
        values=1, 
        lbound=.01, 
        labels=c("varX","varY")
    ),
    mxPath(
        from="X", 
        to="Y", 
        arrows=2, 
        free=T, 
        values=.2, 
        lbound=.01, 
        labels="covXY"
    ),
    mxPath(
        from="one", 
        to=c("X", "Y"), 
        arrows=1, 
        free=T, 
        values=.01, 
        labels=c("meanX","meanY")
    ),
    mxData(
        observed=cov(testData), 
        type="cov", 
        numObs=1000, 
        means=colMeans(testData)
    ),
    type="RAM"
    )
bivSatFit1m <- mxRun(bivSatModel1m)
fitMeasuresMx(bivSatFit1m)
bivSatFit1mSim <- sim(10, bivSatFit1m, n = 200)
summary(bivSatFit1mSim)

########################## Fitting bivSatFit2

bivSatModel2 <- mxModel("bivSat2",
    manifestVars= selVars,
    mxPath(
        from=c("X", "Y"), 
        arrows=2, 
        free=T, 
        values=1, 
        lbound=.01, 
        labels=c("varX","varY")
    ),
    mxPath(
        from="X", 
        to="Y", 
        arrows=2, 
        free=T, 
        values=.2, 
        lbound=.01, 
        labels="covXY"
    ),
    mxPath(from="one", 
    	to=c("X", "Y"),
        arrows=1, 
        free=T), 
    mxData(
        observed=testData, 
        type="raw", 
    ),
    type="RAM"
    )

bivSatFit2 <- mxRun(bivSatModel2)
fitMeasuresMx(bivSatFit2)
bivSatFit2Sim <- sim(10, bivSatFit2, n = 200)
summary(bivSatFit2Sim)

################# Fitting bivSatFit2s

bivSatModel2s <- mxModel(bivSatModel1m,
    mxData(
        observed=testData, 
        type="raw", 
    ),
    mxPath(from="one", 
    	to=c("X", "Y"),
        arrows=1, 
        free=T),     
    name = "bivSat2s"
    )
bivSatFit2s <- mxRun(bivSatModel2s)
fitMeasuresMx(bivSatFit2s)
bivSatFit2sSim <- sim(10, bivSatFit2s, n = 200)
summary(bivSatFit2sSim)

######################## Fitting bivSatFit3m

# The bivSatFit3 object does not work with simsem because the expected mean is not specified.

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
    mxMLObjective(
        covariance="expCov",
        means="expMean",
        dimnames=selVars
    )
    )
bivSatFit3m <- mxRun(bivSatModel3m)
fitMeasuresMx(bivSatFit3m)
bivSatFit3mSim <- sim(10, bivSatFit3m, n = 200)
summary(bivSatFit3mSim)

################ Fitting bivSatFit4

bivSatModel4 <- mxModel("bivSat4",
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
    observed=testData, 
    type="raw", 
),
mxFIMLObjective(
    covariance="expCov",
    means="expMean",
    dimnames=selVars
)
)
bivSatFit4 <- mxRun(bivSatModel4)
fitMeasuresMx(bivSatFit4)
bivSatFit4Sim <- sim(10, bivSatFit4, n = 200)
summary(bivSatFit4Sim)

####################### Fitting bivSatFit5m

# The bivSatFit5 object does not work with simsem because the expected mean is not specified.

bivSatModel5m <- mxModel("bivSat5m",
    mxMatrix(
        type="Full", 
        nrow=2, 
        ncol=2, 
        free=c(T,T,F,T), 
        values=c(1,.2,0,1), 
        name="Chol"
    ),
    mxAlgebra(
        Chol %*% t(Chol), 
        name="expCov", 
        dimnames=list(selVars,selVars)
    ),
    mxMatrix(
        type="Full", 
        nrow=1, 
        ncol=2, 
        free=T, 
        values=c(0,0), 
        dimnames=list(NULL,selVars), 
        name="expMean"
    ),
    mxData(
        observed=cov(testData), 
        type="cov", 
        numObs=1000, 
        means=colMeans(testData) 
    ),
    mxMLObjective(
        covariance="expCov",
        means="expMean",
    dimnames=selVars
    )
    )
bivSatFit5m <- mxRun(bivSatModel5m)
fitMeasuresMx(bivSatFit5m)
bivSatFit5mSim <- sim(10, bivSatFit5m, n = 200)
summary(bivSatFit5mSim)

###################### Fitting bivSatFit6

bivSatModel6 <- mxModel("bivSat6",
    mxMatrix(
        type="Full", 
        nrow=2, 
        ncol=2, 
        free=c(T,T,F,T), 
        values=c(1,.2,0,1), 
        name="Chol"
    ),
    mxAlgebra(
        Chol %*% t(Chol), 
        name="expCov", 
        dimnames=list(selVars,selVars)
    ),
    mxMatrix(
        type="Full", 
        nrow=1, 
        ncol=2, 
        free=T, 
        values=c(0,0), 
        dimnames=list(NULL,selVars), 
        name="expMean"
    ),
    mxData(
        observed=testData, 
        type="raw", 
    ),
    mxFIMLObjective(
        covariance="expCov",
        means="expMean",
		dimnames=selVars
    )
    )
bivSatFit6 <- mxRun(bivSatModel6)
fitMeasuresMx(bivSatFit6)
bivSatFit6Sim <- sim(10, bivSatFit6, n = 200)
summary(bivSatFit6Sim)
