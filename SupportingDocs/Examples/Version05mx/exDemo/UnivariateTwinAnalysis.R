library(simsem)
library(semTools)
library(OpenMx)
library(MASS)

####################### Fitting twinSatFit

set.seed(200)
a2<-0.5		#Additive genetic variance component (a squared)
c2<-0.3		#Common environment variance component (c squared)
e2<-0.2		#Specific environment variance component (e squared)
rMZ <- a2+c2
rDZ <- .5*a2+c2
DataMZ <- mvrnorm (1000, c(0,0), matrix(c(1,rMZ,rMZ,1),2,2))
DataDZ <- mvrnorm (1000, c(0,0), matrix(c(1,rDZ,rDZ,1),2,2))

selVars <- c('t1','t2')
dimnames(DataMZ) <- list(NULL,selVars)
dimnames(DataDZ) <- list(NULL,selVars)
summary(DataMZ)
summary(DataDZ)
colMeans(DataMZ,na.rm=TRUE)
colMeans(DataDZ,na.rm=TRUE)
cov(DataMZ,use="complete")
cov(DataDZ,use="complete")

twinSatModel <- mxModel("twinSat",
	mxModel("MZ",
		mxMatrix("Full", 1, 2, T, c(0,0), name="expMeanMZ"), 
		mxMatrix("Lower", 2, 2, T, .5, name="CholMZ"), 
		mxAlgebra(CholMZ %*% t(CholMZ), name="expCovMZ"), 
		mxData(DataMZ, type="raw"),
		mxFIMLObjective("expCovMZ", "expMeanMZ", selVars)),  
	mxModel("DZ",
		mxMatrix("Full", 1, 2, T, c(0,0), name="expMeanDZ"), 
		mxMatrix("Lower", 2, 2, T, .5, name="CholDZ"), 
		mxAlgebra(CholDZ %*% t(CholDZ), name="expCovDZ"), 
		mxData(DataDZ, type="raw"), 
		mxFIMLObjective("expCovDZ", "expMeanDZ", selVars)),
	mxAlgebra(MZ.objective + DZ.objective, name="twin"), 
	mxAlgebraObjective("twin"))
twinSatFit <- mxRun(twinSatModel, suppressWarnings=TRUE)
fitMeasuresMx(twinSatFit)
twinSatFitSim <- sim(10, twinSatFit, n = list(100, 100))
summary(twinSatFitSim)

####################### Fitting twinSatFitSub1

twinSatModelSub1 <- twinSatModel
twinSatModelSub1$MZ$expMeanMZ <- mxMatrix("Full", 1, 2, T, 0, "mMZ", name="expMeanMZ")
twinSatModelSub1$DZ$expMeanDZ <- mxMatrix("Full", 1, 2, T, 0, "mDZ", name="expMeanDZ")
twinSatFitSub1 <- mxRun(twinSatModelSub1, suppressWarnings=TRUE)
fitMeasuresMx(twinSatFitSub1)
twinSatFitSub1Sim <- sim(10, twinSatFitSub1, n = list(100, 100))
summary(twinSatFitSub1Sim)

####################### Fitting twinSatFitSub2

twinSatModelSub2 <- twinSatModelSub1
twinSatModelSub2$MZ$expMeanMZ <- mxMatrix("Full", 1, 2, T, 0, "mean", name="expMeanMZ")
twinSatModelSub2$DZ$expMeanDZ <- mxMatrix("Full", 1, 2, T, 0, "mean", name="expMeanDZ")
twinSatFitSub2 <- mxRun(twinSatModelSub2, suppressWarnings=TRUE)
fitMeasuresMx(twinSatFitSub2)
twinSatFitSub2Sim <- sim(10, twinSatFitSub2, n = list(100, 100))
summary(twinSatFitSub2Sim)

####################### Fitting twinACEFit 

twinACEModel <- mxModel("twinACE", 
	mxMatrix("Full", 1, 2, T, 20, "mean", name="expMean"), 
	mxMatrix("Full", nrow=1, ncol=1, free=TRUE, values=.6, label="a", name="X"),
	mxMatrix("Full", nrow=1, ncol=1, free=TRUE, values=.6, label="c", name="Y"),
	mxMatrix("Full", nrow=1, ncol=1, free=TRUE, values=.6, label="e", name="Z"),
	mxAlgebra(X * t(X), name="A"),
	mxAlgebra(Y * t(Y), name="C"),
	mxAlgebra(Z * t(Z), name="E"),	
	mxAlgebra(rbind(cbind(A+C+E   , A+C),
					cbind(A+C     , A+C+E)), name="expCovMZ"),
	mxModel("MZ",
		mxData(DataMZ, type="raw"), 
		mxFIMLObjective("twinACE.expCovMZ", "twinACE.expMean",selVars)),

	mxAlgebra(rbind(cbind(A+C+E   , .5%x%A+C),
					cbind(.5%x%A+C , A+C+E)), name="expCovDZ"),
	mxModel("DZ", 
		mxData(DataDZ, type="raw"), 
		mxFIMLObjective("twinACE.expCovDZ", "twinACE.expMean",selVars)),

	mxAlgebra(MZ.objective + DZ.objective, name="twin"), 
	mxAlgebraObjective("twin"))
twinACEFit <- mxRun(twinACEModel, suppressWarnings=TRUE)
fitMeasuresMx(twinACEFit)
twinACEFitSim <- sim(10, twinACEFit, n = list(100, 100))
summary(twinACEFitSim)

####################### Fitting twinAEFit 

twinAEModel <- twinACEModel
twinAEModel$twinACE$Y <- mxMatrix("Full", 1, 1, F, 0, "c", name="Y")  # drop c
twinAEFit <- mxRun(twinAEModel, suppressWarnings=TRUE)
fitMeasuresMx(twinAEFit)
twinAEFitSim <- sim(10, twinAEFit, n = list(100, 100))
summary(twinAEFitSim)
