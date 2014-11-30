library(simsem)
library(semTools)
library(OpenMx)
library(MASS)

################### Fitting bivSatFit4

set.seed(200)
rs=.5
xy <- mvrnorm (1000, c(0,0), matrix(c(1,rs,rs,1),2,2))
testData <- xy
selVars <- c("X","Y")
dimnames(testData) <- list(NULL, selVars)
summary(testData)
cov(testData)
# Simulate Data
# -----------------------------------------------------------------------------

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
	mxExpectationNormal(
	    covariance="expCov",
	    means="expMean",
	    dimnames=selVars
	)
)

bivSatFit4 <- mxRun(bivSatModel4)
fitMeasuresMx(bivSatFit4)
bivSatFit4Sim <- sim(10, bivSatFit4, n = 200)
summary(bivSatFit4Sim)
