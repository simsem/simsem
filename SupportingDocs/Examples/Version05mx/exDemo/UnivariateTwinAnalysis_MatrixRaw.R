library(simsem)
library(semTools)
library(OpenMx)

####################### Fitting twinACEFit

data(twinData)
summary(twinData)
selVars <- c('bmi1','bmi2')
mzData <- as.matrix(subset(twinData, zyg==1, c(bmi1,bmi2)))
dzData <- as.matrix(subset(twinData, zyg==3, c(bmi1,bmi2)))
colMeans(mzData,na.rm=TRUE)
colMeans(dzData,na.rm=TRUE)
cov(mzData,use="complete")
cov(dzData,use="complete")

twinACEModel <- mxModel("twinACE",
	mxMatrix(
		type="Full", 
		nrow=1, 
		ncol=1, 
		free=TRUE,  
		values=.6,  
		label="a", 
		name="X"
	), 
	mxMatrix(
		type="Full", 
		nrow=1, 
		ncol=1, 
		free=TRUE,  
		values=.6,  
		label="c", 
		name="Y"
	),
	mxMatrix(
		type="Full", 
		nrow=1, 
		ncol=1, 
		free=TRUE,  
		values=.6,  
		label="e", 
		name="Z"
	),
	mxAlgebra(
		expression=X %*% t(X), 
		name="A"
	), 
	mxAlgebra(
		expression=Y %*% t(Y), 
		name="C"
	), 
	mxAlgebra(
		expression=Z %*% t(Z), 
		name="E"
	),
	mxMatrix(
		type="Full", 
		nrow=1, 
		ncol=2, 
		free=TRUE, 
		values= 20,
		label="mean", 
		name="expMean"
	),
    mxAlgebra(
		expression= rbind  (cbind(A+C+E , A+C),
							cbind(A+C   , A+C+E)), 
		name="expCovMZ"
	),
    mxAlgebra(
		expression= rbind  (cbind(A+C+E     , 0.5%x%A+C),
							cbind(0.5%x%A+C , A+C+E)), 
		name="expCovDZ"
	),
	mxModel("MZ",
	    mxData(
			observed=mzData, 
			type="raw"
		), 
	    mxFIMLObjective(
			covariance="twinACE.expCovMZ", 
			means="twinACE.expMean", 
			dimnames=selVars
		)
	),
	mxModel("DZ",
	    mxData(
			observed=dzData, 
			type="raw"
		), 
	    mxFIMLObjective(
			covariance="twinACE.expCovDZ", 
			means="twinACE.expMean", 
			dimnames=selVars
		)
	),
    mxAlgebra(
		expression=MZ.objective + DZ.objective, 
		name="twin"
	), 
	mxAlgebraObjective("twin")
)

twinACEFit <- mxRun(twinACEModel)
fitMeasuresMx(twinACEFit)
twinACEFitSim <- sim(10, twinACEFit, n = list(100, 100))
summary(twinACEFitSim)

########################### Fitting twinAEFit

twinAEModel <- mxModel(twinACEModel, #twinAE model so we drop c at 0
	mxMatrix(
		type="Full", 
		nrow=1, 
		ncol=1, 
		free=FALSE, 
		values=0, 
		label="c", 
		name="Y"
	)	
)

twinAEFit <- mxRun(twinAEModel)
fitMeasuresMx(twinAEFit)
twinAEFitSim <- sim(10, twinAEFit, n = list(100, 100))
summary(twinAEFitSim)
