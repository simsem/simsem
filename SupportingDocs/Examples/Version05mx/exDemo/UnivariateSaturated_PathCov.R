library(simsem)
library(semTools)
library(OpenMx)

###################### Fitting univSatFit1m

# THe univSatFit1 object does not work with simsem because the expected mean is not specified.

set.seed(100)
x <- rnorm (1000, 0, 1)
testData <- as.matrix(x)
selVars <- c("X")
dimnames(testData) <- list(NULL, selVars)
summary(testData)
colMeans(testData)
var(testData)

univSatModel1m <- mxModel("univSat1m",
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
