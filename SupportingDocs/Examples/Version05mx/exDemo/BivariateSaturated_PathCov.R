library(simsem)
library(semTools)
library(OpenMx)
library(MASS)

##################### Fitting bivSatFit1m

# The bivSatFit1 object does not work with simsem becuase the expected mean is not specified.

set.seed(200)
rs=.5
xy <- mvrnorm (1000, c(0,0), matrix(c(1,rs,rs,1),2,2))
testData <- xy
selVars <- c("X","Y")
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
