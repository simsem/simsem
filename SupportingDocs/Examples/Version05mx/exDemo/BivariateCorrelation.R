library(simsem)
library(semTools)
library(OpenMx)
library(MASS)

##################### Fitting bivCorFit

set.seed(200)
rs=.5
xy <- mvrnorm (1000, c(0,0), matrix(c(1,rs,rs,1),2,2))
testData <- xy
selVars <- c('X','Y')
dimnames(testData) <- list(NULL, selVars)
summary(testData)
cov(testData)

bivCorModel <- mxModel("bivCor",
    mxMatrix(
        type="Full", 
        nrow=1, 
        ncol=2, 
        free=TRUE, 
        values=c(0,0), 
        name="expMean"
    ), 
    mxMatrix(
        type="Lower", 
        nrow=2, 
        ncol=2, 
        free=TRUE,
        values=.5, 
        name="Chol"
    ), 
    mxAlgebra(
        expression=Chol %*% t(Chol), 
        name="expCov", 
    ), 
    mxData(
        observed=testData, 
        type="raw"
    ), 
    mxExpectationNormal(
        covariance="expCov", 
        means="expMean",
        dimnames=selVars),
	mxFitFunctionML()
)


bivCorFit <- mxRun(bivCorModel)
fitMeasuresMx(bivCorFit)
bivCorFitSim <- sim(10, bivCorFit, n = 200)
summary(bivCorFitSim)

################## Fitting bivCorFitSub

bivCorModelSub <-mxModel(bivCorModel,
    mxMatrix(
        type="Diag", 
        nrow=2, 
        ncol=2, 
        free=TRUE, 
		values=c(1,1),
        name="Chol"
    )
)

bivCorFitSub <- mxRun(bivCorModelSub)
fitMeasuresMx(bivCorFitSub)
bivCorFitSubSim <- sim(10, bivCorFitSub, n = 200)
summary(bivCorFitSubSim)
