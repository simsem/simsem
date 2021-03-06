library(simsem)
library(semTools)
library(OpenMx)

################### Fitting oneFactorFit

myFADataCov <- matrix(
	c(0.997, 0.642, 0.611, 0.672, 0.637, 0.677,
	  0.642, 1.025, 0.608, 0.668, 0.643, 0.676,
	  0.611, 0.608, 0.984, 0.633, 0.657, 0.626,
	  0.672, 0.668, 0.633, 1.003, 0.676, 0.665,
	  0.637, 0.643, 0.657, 0.676, 1.028, 0.654,
	  0.677, 0.676, 0.626, 0.665, 0.654, 1.020),
	nrow=6,
	dimnames=list(
		c("x1","x2","x3","x4","x5","x6"),
		c("x1","x2","x3","x4","x5","x6"))
)

myFADataMeans <- c(2.988, 3.011, 2.986, 3.053, 3.016, 3.010)
names(myFADataMeans) <- c("x1","x2","x3","x4","x5","x6")

oneFactorModel <- mxModel("Common Factor Model Matrix Specification", 
	mxData(
		observed=myFADataCov, 
		type="cov", 
		numObs=500,
		mean=myFADataMeans
	),
	mxMatrix(
		type="Full", 
		nrow=7, 
		ncol=7,
		values=c(0,0,0,0,0,0,1,
		         0,0,0,0,0,0,1,
		         0,0,0,0,0,0,1,
		         0,0,0,0,0,0,1,
		         0,0,0,0,0,0,1,
		         0,0,0,0,0,0,1,
		         0,0,0,0,0,0,0),
		free=c(F, F, F, F, F, F, F,
		       F, F, F, F, F, F, T,
		       F, F, F, F, F, F, T,
		       F, F, F, F, F, F, T,
		       F, F, F, F, F, F, T,
		       F, F, F, F, F, F, T,
		       F, F, F, F, F, F, F),
		labels=c(NA,NA,NA,NA,NA,NA,"l1",
		         NA,NA,NA,NA,NA,NA,"l2",
		         NA,NA,NA,NA,NA,NA,"l3",
		         NA,NA,NA,NA,NA,NA,"l4",
		         NA,NA,NA,NA,NA,NA,"l5",
		         NA,NA,NA,NA,NA,NA,"l6",
		         NA,NA,NA,NA,NA,NA,NA),
		byrow=TRUE,
		name="A"
	),
	mxMatrix(
		type="Symm", 
		nrow=7, 
		ncol=7, 
		values=c(1,0,0,0,0,0,0,
		         0,1,0,0,0,0,0,
		         0,0,1,0,0,0,0,
		         0,0,0,1,0,0,0,
		         0,0,0,0,1,0,0,
		         0,0,0,0,0,1,0,
		         0,0,0,0,0,0,1),
		free=c(T, F, F, F, F, F, F,
		       F, T, F, F, F, F, F,
		       F, F, T, F, F, F, F,
		       F, F, F, T, F, F, F,
		       F, F, F, F, T, F, F,
		       F, F, F, F, F, T, F,
		       F, F, F, F, F, F, T),
		labels=c("e1", NA,   NA,   NA,   NA,   NA,   NA,
		         NA, "e2",   NA,   NA,   NA,   NA,   NA,
		         NA,   NA, "e3",   NA,   NA,   NA,   NA,
		         NA,   NA,   NA, "e4",   NA,   NA,   NA,
		         NA,   NA,   NA,   NA, "e5",   NA,   NA,
		         NA,   NA,   NA,   NA,   NA, "e6",   NA,
		         NA,   NA,   NA,   NA,   NA,   NA, "varF1"),
		byrow=TRUE,
		name="S"
	),
	mxMatrix(
		type="Full", 
		nrow=6, 
		ncol=7,
		free=FALSE,
		values=c(1,0,0,0,0,0,0,
		         0,1,0,0,0,0,0,
		         0,0,1,0,0,0,0,
		         0,0,0,1,0,0,0,
		         0,0,0,0,1,0,0,
		         0,0,0,0,0,1,0),
		byrow=TRUE,
		name="F"
	),
	mxMatrix(
		type="Full", 
		nrow=1, 
		ncol=7,
		values=c(1,1,1,1,1,1,0),
		free=c(T,T,T,T,T,T,F),
		labels=c("meanx1","meanx2","meanx3",
		         "meanx4","meanx5","meanx6",
		         NA),
		name="M"
	),
	mxRAMObjective("A","S","F","M",dimnames=c("x1","x2","x3","x4","x5","x6","F1"))
)
   
oneFactorFit <- mxRun(oneFactorModel)
fitMeasuresMx(oneFactorFit)
oneFactorFitSim <- sim(10, oneFactorModel, n = 200)
summary(oneFactorFitSim)
