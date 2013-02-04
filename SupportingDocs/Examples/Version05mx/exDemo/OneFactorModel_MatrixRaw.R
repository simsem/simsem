library(simsem)
library(semTools)
library(OpenMx)

############### Fitting oneFactorFit

data(myFADataRaw)

manifestVars <- c("x1","x2","x3","x4","x5","x6")
latentVars <- "F1"

myFADataRaw <- myFADataRaw[,manifestVars]

oneFactorModel <- mxModel("Common Factor Model Matrix Specification", 
	mxData(
		observed=myFADataRaw, 
		type="raw"
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
	mxRAMObjective("A","S","F","M",
		dimnames=c(manifestVars, latentVars))
)
    
oneFactorFit <- mxRun(oneFactorModel)
fitMeasuresMx(oneFactorFit)
oneFactorFitSim <- sim(10, oneFactorFit, n = 200)
summary(oneFactorFitSim)
