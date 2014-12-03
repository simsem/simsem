library(simsem)
library(semTools)
library(OpenMx)
library(MASS)

######################## Fitting bivSatFit6

set.seed(200)
rs=.5
xy <- mvrnorm (1000, c(0,0), matrix(c(1,rs,rs,1),2,2))
testData <- xy
selVars <- c("X","Y")
dimnames(testData) <- list(NULL, selVars)
summary(testData)
cov(testData)

bivSatModel6 <- mxModel("bivSat6",
    mxMatrix(
		type="Full", 
		nrow=2, 
		ncol=2, 
		free=c(TRUE,TRUE,FALSE,TRUE), 
		values=c(1,.2,0,1), 
		name="Chol"
	),
	mxMatrix(
		type="Full", 
		nrow=1, 
		ncol=2, 
		free=TRUE, 
		values=c(0,0), 
		name="expMean"
	),
    mxAlgebra(
		expression=Chol %*% t(Chol), 
		name="expCov"
	),
	mxData(
		observed=testData, 
		type="raw"
	),
	mxExpectationNormal(
		covariance="expCov",
		means="expMean",
		dimnames=selVars
	),
	mxFitFunctionML()
)

bivSatFit6 <- mxRun(bivSatModel6)
fitMeasuresMx(bivSatFit6)
bivSatFit6Sim <- sim(10, bivSatFit6, n = 200)
summary(bivSatFit6Sim)
