###################################################################
# Distribution Classes
# Classes -- simsem package
# Object that create a random number from a distribution.
# Parent Class: VirtualDist
# Child Class:	None
# Methods:
#	run
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: February 23, 2011

setClass("SimUnif",
	representation(
		min="numeric",
		max="numeric"
	)
)
# SimUnif: Uniform Distribution
# Attributes: min = lower bound, max = upper bound (min < max)

setClass("SimNorm",
	representation(
		mean="numeric",
		sd="numeric"
	)
)
# SimNorm: Normal Distribution
# Attributes: mean = population mean, sd = population standard deviation (> 0)

setClass("SimBeta",
	representation(
		shape1="numeric",
		shape2="numeric",
		ncp="numeric"
	),
	prototype(ncp=0)
)
# SimBeta: Beta Distribution
# Attributes: shape1, shape2 = positive numbers of beta distributions, ncp = non-centrality parameter (shape1, shape2 > 0)

setClass("SimBinom",
	representation(
		size="numeric",
		prob="numeric"
	)
)
# SimBinom: Binomial Distribution
# Attributes: size = Number of trials (zero or more), prob = probability of success on each trial (0 to 1)

setClass("SimCauchy",
	representation(
		location="numeric",
		scale="numeric"
	),
	prototype(location=0, scale=1)
)
# SimCauchy: Cauchy Distribution
# Attributes: location = location parameter, scale = scale parameter (> 0)

setClass("SimChisq",
	representation(
		df="numeric",
		ncp="numeric"
	),
	prototype(ncp=0)
)
# SimChisq: Chi-squared Distribution
# Attributes: df = degrees of freedom (non-negative), ncp = non-centrality parameter (non-negative)

setClass("SimExp",
	representation(
		rate="numeric"
	),
	prototype(rate=1)
)
# SimExp: Exponential Distribution
# Attributes: rate = rate parameter

setClass("SimF",
	representation(
		df1="numeric",
		df2="numeric",
		ncp="numeric"
	),
	prototype(ncp=0)
)
# SimF: F-distribution
# Attributes: df1, df2 = degrees of freedom (>0), ncp = non-centrality parameter (> 0)

setClass("SimGamma",
	representation(
		shape="numeric",
		rate="numeric"
	),
	prototype(rate=1)
)
# SimGamma: Gamma Distribution
# Attributes: shape = Shape parameter, scale = Scale parameter

setClass("SimGeom",
	representation(
		prob="numeric"
	)
)
# SimGeom: Geometric Distribution
# Attributes: prob = probability of successes

setClass("SimHyper",
	representation(
		m="numeric",
		n="numeric",
		k="numeric"
	)
)
# SimHyper: Hypergeometric Distribution
# Attributes: m = The number of successes, n = The number of failures, k =  The number of drawns (All are integers)

setClass("SimLnorm",
	representation(
		meanlog="numeric",
		sdlog="numeric"
	),
	prototype(meanlog=0, sdlog=1)
)
# SimLnorm: Log Normal Distribution
# Attributes: meanlog = mean of the distribution in log scale, sdlog = standard deviation of the distribution in log scale (sdlog > 0)

setClass("SimLogis",
	representation(
		location="numeric",
		scale="numeric"
	),
	prototype(location=0, scale=1)
)
# SimLogis: Logistic Distribution
# Attributes: location = location parameter, scale = scale parameter (> 0)

setClass("SimNbinom",
	representation(
		size="numeric",
		prob="numeric"
	)
)
# SimNbinom: Negative Binomial Distribution
# Attributes: size = Target for number of sucessful trials (> 0), prob = probability of each trials (0 < p < 1)

setClass("SimPois",
	representation(
		lambda="numeric"
	)
)
# SimPois: Poisson Distribution
# Attributes: lambda = mean and variance (> 0)

setClass("SimT",
	representation(
		df="numeric",
		ncp="numeric"
	),
	prototype(ncp=0)
)
# SimT: Student t Distribution
# Attributes: df = degree of freedom (> 0), ncp = non-centrality parameter

setClass("SimWeibull",
	representation(
		shape="numeric",
		scale="numeric"
	),
	prototype(scale=1)
)
# SimWeibull: Weibull Distribution
# Attributes: shape = shape parameter, scale = scale parameter (> 0)

###################################################################
# VirtualDist
# Class -- simsem package
# Virtual class that aggregates all distribution objects together.
# Constructor:	None
# Parent Class: None
# Child Class: SimNorm, SimUnif
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClassUnion("VirtualDist", c("SimUnif", "SimNorm", "SimBeta", "SimBinom", "SimCauchy", "SimChisq", "SimExp", "SimF", "SimGamma", "SimGeom", "SimHyper", "SimLnorm", "SimLogis", "SimNbinom", "SimPois", "SimT", "SimWeibull"))

###################################################################
# SimMatrix
# Class -- simsem package
# This object can be used to represent a matrix in SEM model. It contains free parameters, fixed values, and starting values. 
# This object can be represented factor loading matrix or regreesion coefficient matrix. 
# Constructor:	simMatrix(free, param=NULL)
# Parent Class: None
# Child Class:	SymMatrix, NullSimMatrix, NullSymMatrix (2 generation)
# Attributes:
#	free: 		Free parameters as NA or values of fixed parameters
# 	param: 	All population/starting values of those free parameters
# Methods:
#	adjust
#	combineObject
#	countRandomObject
#	isNullObject
#	run
#	startingValues
#	summaryShort
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("SimMatrix", 
	representation(
		free="matrix",
		param="matrix"
	), 
	prototype(free=as.matrix(NaN), param=as.matrix(NaN))
)
#Examples:
#showClass("SimMatrix")
#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#loadingValues[1:3, 1] <- 0.7
#loadingValues[4:6, 2] <- 0.7
#LX <- simMatrix(loading, loadingValues)
#summary(LX)
#run(LX)
#n65 <- simNorm(0.6, 0.05)
#LY <- simMatrix(loading, "n65")
#summary(LY)
#run(LY)
#u34 <- simUnif(0.3, 0.4)
#LY <- adjust(LY, "u34", c(2, 1))
#summary(LY)
#run(LY)
#summaryShort(LY)

###################################################################
# SymMatrix
# Class -- simsem package
# This object can be used to represent a symmetric matrix in SEM model. It contains free parameters, fixed values, and starting values. 
# This object can be represented factor correlation or error correlation matrix.
# Constructor:	symMatrix(free, param=NULL)
# Parent Class: None
# Child Class:	NullSymMatrix
# Attributes:
#	free: 		Free parameters as NA or values of fixed parameters
# 	param: 	All population/starting values of those free parameters
# Methods:
#	adjust
#	countRandomObject
#	isNullObject
#	run
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("SymMatrix",
	contains = "SimMatrix"
)
#Examples:
#showClass("SymMatrix")
#latent.cor <- matrix(NA, 3, 3)
#diag(latent.cor) <- 1
#RPH <- symMatrix(latent.cor, 0.5)
#u46 <- simUnif(0.4, 0.6)
#RPH <- adjust(RPH, "u46", c(3,2))
#summary(RPH)
#summaryShort(RPH)
#run(RPH)

###################################################################
# SimVector
# Class -- simsem package
# This object can be used to represent a vector in SEM model. It contains free parameters, fixed values, and starting values. 
# This object can be represented mean, intercept, or variance vectors.
# Constructor:	simVector(free, param=NULL)
# Parent Class: None
# Child Class:	NullSimVector
# Attributes:
#	Data: 		Free parameters as NA or values of fixed parameters
# 	Labels: 	All population/starting values of those free parameters
# Methods:
#	adjust
#	combineObject
#	countRandomObject
#	isNullObject
#	run
#	startingValues
#	summaryShort
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("SimVector", 
	representation(
		free="vector",
		param="vector"
	), 
	prototype(free=as.vector(NaN), param=as.vector(NaN))
)
#Examples:
#showClass("SimVector")
#factor.mean <- rep(NA, 2)
#factor.mean.starting <- c(5, 2)
#AL <- simVector(factor.mean, factor.mean.starting)
#run(AL)
#summary(AL)
#summaryShort(AL)
#n01 <- simNorm(0, 1)
#AL <- adjust(AL, "n01", 2)
#run(AL)
#summary(AL)

###################################################################
# NullVector
# Class -- simsem package
# The null object of vector.c
# Constructor:	new("NullVector")
# Parent Class: vector
# Child Class:	None
# Attributes:	Zero length vector
#		Does not matter because these attributes will not be used. 
#		It will be checked whether the class is NULL only.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("NullVector", contains = "vector")

###################################################################
# NullMatrix
# Class -- simsem package
# The null object of matrix.c
# Constructor:	new("NullMatrix")
# Parent Class: matrix
# Child Class:	None
# Attributes:	Matrix with 0 x 0 dimension
#		Does not matter because these attributes will not be used. 
#		It will be checked whether the class is NULL only.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("NullMatrix", contains = "matrix")

###################################################################
# NullSimMatrix
# Class -- simsem package
# The null object of SimMatrix.c
# Constructor:	new("NullSimMatrix")
# Parent Class: SimMatrix
# Child Class:	None
# Attributes:	SimMatrix with NaN in both attributes
#		Does not matter because these attributes will not be used. 
#		It will be checked whether the class is NULL only.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("NullSimMatrix", contains="SimMatrix")

###################################################################
# NullSymMatrix
# Class -- simsem package
# The null object of SymMatrix.c
# Constructor:	new("NullSymMatrix")
# Parent Class: SymMatrix
# Child Class:	None
# Attributes:	SymMatrix with NaN in both attributes
#		Does not matter because these attributes will not be used. 
#		It will be checked whether the class is NULL only.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("NullSymMatrix", contains="SymMatrix")

###################################################################
# NullSimVector
# Class -- simsem package
# The null object of SimVector.c
# Constructor:	new("NullSimVector")
# Parent Class: SimVector
# Child Class:	None
# Attributes:	SimVector with NaN in both attributes
#		Does not matter because these attributes will not be used. 
#		It will be checked whether the class is NULL only.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("NullSimVector", contains="SimVector")

###################################################################
# SimSet
# Class -- simsem package
# Set of SimVector.c and SimMatrix.c that saves model specification (CFA, Path analysis, or SEM)
# Constructor:	
#	simSetCFA(...) for CFA
#	simSetPath(...) for Path analysis
#	simSetSEM(...)	for SEM
# Parent Class: None
# Child Class:	NullSimSet, SimMisspec, NullSimMisspec (2 generations)
# Attributes:
#	modelType:	Model type (CFA, Path, or SEM)
#	LY:		SimMatrix.c of Factor loading matrix between endogenous factors and Y indicators 
#	TE:
#	RTE:		SymMatrix.c of Correlation matrix between Y measurement error 
#	VTE:	SimVector.c of Variance of Y measurement error 
#	PS:
#	RPS:		SymMatrix.c of Residual correlation of endogenous factors  
#	VPS:	SimVector.c of Residual variances of endogenous factors 
#	BE:		SimMatrix.c of Regression effect among endogenous factors 
#	TY:		SimVector.c of Measurement intercepts of Y indicators 
#	AL:		SimVector.c of Factor intercepts of endogenous factors 
#	ME:		SimVector.c of Factor means of endogenous factors 
#	MY:		SimVector.c of Total mean of Y indicators 
#	VE:		SimVector.c of Total variance of endogenous factors 
#	VY:		SimVector.c of Total variance of Y indicators 
#	LX:		SimMatrix.c of Factor loading matrix between exogenous factors and X indicators 
#	TD:
#	RTD:		SymMatrix.c of Correlation matrix between X measurement error 
#	VTD:	SimVector.c of Variance of X measurement error 
# 	PH:
#	RPH:		SymMatrix.c of Correlation among exogenous factors 
#	GA:		SimMatrix.c of Regreeion effect from exogenous factors to endogenous factors 
#	TX:		SimVector.c of Measurement intercepts of X indicators 
#	KA:		SimVector.c of Factor mean of exogenous factors 
#	MX:		SimVector.c of Total mean of X indicators 
#	VPH:	SimVector.c of Variance of exogenous factors 
#	VX:		SimVector.c of Total variance of X indicators 
#	TH:
#	RTH:		SimMatrix.c Measurement error correlation between X indicators and Y indicators 
# Methods:
#	countRandomObject
#	isNullObject
#	simModel
#	run
#	startingValues
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("SimSet", 
	representation(
		modelType="character", #Path, Path.exo, CFA, SEM, SEM.exo
		LY="SimMatrix",
		TE="SymMatrix",
		RTE="SymMatrix",
		VTE="SimVector",
		PS="SymMatrix",
		RPS="SymMatrix",
		VPS="SimVector",
		BE="SimMatrix",
		TY="SimVector",
		AL="SimVector",
		ME="SimVector",
		MY="SimVector",
		VE="SimVector",
		VY="SimVector",
		LX="SimMatrix",
		TD="SymMatrix",
		RTD="SymMatrix",
		VTD="SimVector",
		PH="SymMatrix",
		RPH="SymMatrix",
		VPH="SimVector",
		GA="SimMatrix",
		TX="SimVector",
		KA="SimVector",
		MX="SimVector",
		VX="SimVector",
		TH="SimMatrix",
		RTH="SimMatrix"), #Delta on rows, epsilon on columns
	prototype(
		LY=new("NullSimMatrix"),
		TE=new("NullSymMatrix"),
		RTE=new("NullSymMatrix"),
		VTE=new("NullSimVector"),
		PS=new("NullSymMatrix"),
		RPS=new("NullSymMatrix"),
		VPS=new("NullSimVector"),
		BE=new("NullSimMatrix"),
		TY=new("NullSimVector"),
		AL=new("NullSimVector"),
		ME=new("NullSimVector"),
		MY=new("NullSimVector"),
		VE=new("NullSimVector"),
		VY=new("NullSimVector"), 
		LX=new("NullSimMatrix"),
		TD=new("NullSymMatrix"),
		RTD=new("NullSymMatrix"),
		VTD=new("NullSimVector"),
		PH=new("NullSymMatrix"),
		RPH=new("NullSymMatrix"),
		VPH=new("NullSimVector"),
		GA=new("NullSimMatrix"),
		TX=new("NullSimVector"),
		KA=new("NullSimVector"),
		MX=new("NullSimVector"),
		VX=new("NullSimVector"),
		RTH=new("NullSimMatrix"))
)
#Examples:
#showClass("SimSet")
#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#loadingValues[1:3, 1] <- 0.7
#loadingValues[4:6, 2] <- 0.7
#LX <- simMatrix(loading, loadingValues)
#summary(LX)
#latent.cor <- matrix(NA, 2, 2)
#diag(latent.cor) <- 1
#RPH <- symMatrix(latent.cor, 0.5)
#error.cor <- matrix(0, 6, 6)
#diag(error.cor) <- 1
#RTD <- symMatrix(error.cor)
#CFA.Model <- simSetCFA(LX = LX, RPH = RPH, RTD = RTD)
#summary(CFA.Model)
#run(CFA.Model)

###################################################################
# NullSimSet
# Class -- simsem package
# The null object of SimSet.c
# Constructor:	new("NullSimSet")
# Parent Class: SimSet
# Child Class:	None
# Attributes:	SimSet with null object in all attributes.
#		It does not matter because these attributes will not be used. 
#		It will be checked whether the class is NULL only.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("NullSimSet", contains="SimSet")

###################################################################
# MatrixSet
# Class -- simsem package
# Set of vectors and matrices that saves model specification (CFA, Path analysis, or SEM). 
# This class is the result of running SimSet.c. This will be a sample of all parameters of distribution objects.
# Constructor:	run(SimSet)
# Parent Class: None
# Child Class:	MisspecSet
# Attributes:
#	modelType:	Model type (CFA, Path, or SEM)
#	LY:		matrix.c of Factor loading matrix between endogenous factors and Y indicators 
#	TE:
#	RTE:		matrix.c of Correlation matrix between Y measurement error 
#	VTE:	vector.c of Variance of Y measurement error 
#	PS:
#	RPS:		matrix.c of Residual correlation of endogenous factors  
#	VPS:	vector.c of Residual variances of endogenous factors 
#	BE:		matrix.c of Regression effect among endogenous factors 
#	TY:		vector.c of Measurement intercepts of Y indicators 
#	AL:		vector.c of Factor intercepts of endogenous factors 
#	ME:		vector.c of Factor means of endogenous factors 
#	MY:		vector.c of Total mean of Y indicators 
#	VE:		vector.c of Total variance of endogenous factors 
#	VY:		vector.c of Total variance of Y indicators 
#	LX:		matrix.c of Factor loading matrix between exogenous factors and X indicators 
#	TD:
#	RTD:		matrix.c of Correlation matrix between X measurement error 
#	VTD:	vector.c of Variance of X measurement error 
#	PH:
#	RPH:		matrix.c of Correlation among exogenous factors 
#	GA:		matrix.c of Regreeion effect from exogenous factors to endogenous factors 
#	TX:		vector.c of Measurement intercepts of X indicators 
#	KA:		vector.c of Factor mean of exogenous factors 
#	MX:		vector.c of Total mean of X indicators 
#	VPH:	vector.c of Variance of exogenous factors 
#	VX:		vector.c of Total variance of X indicators 
#	TH:
#	RTH:		matrix.c Measurement error correlation between X indicators and Y indicators 
# Methods:
#	combineObject
#	createImpliedMACS
#	divideObject
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("MatrixSet", 
	representation(
		modelType="character",
		LY="matrix",
		RTE="matrix",
		TE="matrix",
		VTE="vector",
		PS="matrix",
		RPS="matrix",
		VPS="vector",
		BE="matrix",
		TY="vector",
		AL="vector",
		ME="vector",
		MY="vector",
		VE="vector",
		VY="vector",
		LX="matrix",
		TD="matrix",
		RTD="matrix",
		VTD="vector",
		PH="matrix",
		RPH="matrix",
		GA="matrix",
		TX="vector",
		KA="vector",
		MX="vector",
		VPH="vector",
		VX="vector",
		TH="matrix",
		RTH="matrix"),
	prototype(
		LY=new("NullMatrix"),
		TE=new("NullMatrix"),
		RTE=new("NullMatrix"),
		VTE=new("NullVector"),
		PS=new("NullMatrix"),
		RPS=new("NullMatrix"),
		VPS=new("NullVector"),
		BE=new("NullMatrix"),
		TY=new("NullVector"),
		AL=new("NullVector"),
		ME=new("NullVector"),
		MY=new("NullVector"),
		VE=new("NullVector"),
		VY=new("NullVector"),
		LX=new("NullMatrix"),
		TD=new("NullMatrix"),
		RTD=new("NullMatrix"),
		VTD=new("NullVector"),
		PH=new("NullMatrix"),
		RPH=new("NullMatrix"),
		GA=new("NullMatrix"),
		TX=new("NullVector"),
		KA=new("NullVector"),
		MX=new("NullVector"),
		VPH=new("NullVector"),
		VX=new("NullVector"),
		TH=new("NullMatrix"),
		RTH=new("NullMatrix"))
)
#Examples:
#showClass("SimSet")
#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#loadingValues[1:3, 1] <- 0.7
#loadingValues[4:6, 2] <- 0.7
#LX <- simMatrix(loading, loadingValues)
#latent.cor <- matrix(NA, 2, 2)
#diag(latent.cor) <- 1
#RPH <- symMatrix(latent.cor, 0.5)
#error.cor <- matrix(0, 6, 6)
#diag(error.cor) <- 1
#RTD <- symMatrix(error.cor)
#CFA.Model <- simSetCFA(LX = LX, RPH = RPH, RTD = RTD)
#MatrixSet <- run(CFA.Model)
#summary(MatrixSet)

###################################################################
# VirtualRSet
# Class -- simsem package
# Set of vectors and matrices arrangements that will save free parameters, labels, or numbers on its child class. 
# This class will collapse those separation between mean and intercept into intercept only, as well as variance and correlation into covariance matrix only.
# Constructor:	Depends on its child class
# Parent Class: None
# Child Class:	SimFreeParam, SimLabels, SimRSet
# Attributes:
#	modelType:	Model type (CFA, Path, or SEM)
#	LY:		matrix.c of Factor loading matrix between endogenous factors and Y indicators 
#	TE:		matrix.c of Covariance matrix between Y measurement error 
#	PS:		matrix.c of Residual covariance of endogenous factors  
#	BE:		matrix.c of Regression effect among endogenous factors 
#	TY:		vector.c of Measurement intercepts of Y indicators 
#	AL:		vector.c of Factor intercepts of endogenous factors 
#	LX:		matrix.c of Factor loading matrix between exogenous factors and X indicators 
#	TD:		matrix.c of Covariance matrix between X measurement error 
#	PH:		matrix.c of Covariance among exogenous factors 
#	GA:		matrix.c of Regreeion effect from exogenous factors to endogenous factors 
#	TX:		vector.c of Measurement intercepts of X indicators 
#	KA:		vector.c of Factor mean of exogenous factors 
#	TH:		matrix.c Measurement error covariance between X indicators and Y indicators 
# Methods:
#	constrainMatrices
#	tagHeaders
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("VirtualRSet", 
	representation(
		modelType="character",
		LY="matrix",
		TE="matrix",
		PS="matrix",
		BE="matrix",
		TY="vector",
		AL="vector",
		LX="matrix",
		TD="matrix",
		PH="matrix",
		GA="matrix",
		TX="vector",
		KA="vector",
		TH="matrix"),
	prototype(
		LY=new("NullMatrix"),
		TE=new("NullMatrix"),
		PS=new("NullMatrix"),
		BE=new("NullMatrix"),
		TY=new("NullVector"),
		AL=new("NullVector"),
		LX=new("NullMatrix"),
		TD=new("NullMatrix"),
		PH=new("NullMatrix"),
		GA=new("NullMatrix"),
		TX=new("NullVector"),
		KA=new("NullVector"),
		TH=new("NullMatrix"))
)

###################################################################
# SimEqualCon
# Class -- simsem package
# Set of equality constraints that users wish to specify
# Constructor:	simEqualCon(..., modelType)
# Parent Class: None
# Child Class:	nullSimConstrint
# Attributes:
#	con:	List of equality constraint. Each element in the list is an individual equality constraint saved in a matrix.
#			Each row represents each element. If the matrix has two columns, the first column indicates row of the element and 
#			the second column indicates column of the element. If the matrix has three columns, the first column is the group
#			of matrix. The rest is row and column. Row name represents the matrix that the element is in. The definition of row
#			name can be seen in simSetCFA, simSetPath, or simSetSEM, depending on analysis model you specify.
#	modelType:	Analysis model (CFA, SEM, Path)
# Methods:
#	constrainMatrices(list, SimEqualCon)
#	constrainMatrices(VirtualRSet, SimEqualCon)
#	isNullObject
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("SimEqualCon", 
	representation(
		con="list",
		modelType="character")
)
#Examples:
#constraint1 <- matrix(1, 3, 2)
#constraint1[,1] <- 1:3
#rownames(constraint1) <- rep("LY", 3)
#constraint2 <- matrix(2, 3, 2)
#constraint2[,1] <- 4:6
#rownames(constraint2) <- rep("LY", 3)
#constraint3 <- matrix(3, 2, 2)
#constraint3[,1] <- 7:8
#rownames(constraint3) <- rep("LY", 2)
#equal.loading <- simEqualCon(constraint1, constraint2, constraint3, modelType="SEM")

###################################################################
# NullSimEqualCon
# Class -- simsem package
# The null object of SimEqualCon.c
# Constructor:	new("NullSimEqualCon")
# Parent Class: simConstriant
# Child Class:	None
# Attributes:	SimEqualCon with nothing in all attributes.
#		It does not matter because these attributes will not be used. 
#		It will be checked whether the class is NULL only.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("NullSimEqualCon", contains="SimEqualCon", 
	representation(
		con="list",
		modelType="character"), 
	prototype(con=list(NA), modelType="NA")
)

###################################################################
# SimREqualCon
# Class -- simsem package
# Set of equality constraints that users wish to specify
# Constructor:	reduceConstraint(SimEqualCon)
# Parent Class: None
# Child Class:	NullSimREqualCon
# Attributes:
#	con:	List of equality constraint. Each element in the list is an individual equality constraint saved in a matrix.
#			Each row represents each element. If the matrix has two columns, the first column indicates row of the element and 
#			the second column indicates column of the element. If the matrix has three columns, the first column is the group
#			of matrix. The rest is row and column. Row name represents the matrix that the element is in. The definition of row
#			name can be seen in VirtualRSet definition.
#	modelType:	Analysis model (CFA, SEM, Path)
# Methods:
#	constrainMatrices(VirtualRSet, SimREqualCon)
#	isNullObject
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("SimREqualCon", 
	representation(
		con="list",
		modelType="character")
)

###################################################################
# NullSimREqualCon
# Class -- simsem package
# The null object of SimREqualCon.c
# Constructor:	new("NullSimREqualCon")
# Parent Class: simReducedConstriant
# Child Class:	None
# Attributes:	SimREqualCon with nothing in all attributes.
#		It does not matter because these attributes will not be used. 
#		It will be checked whether the class is NULL only.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("NullSimREqualCon", contains="SimREqualCon", 
	representation(
		con="list",
		modelType="character"), 
	prototype(con=list(NA), modelType="NA")
)

###################################################################
# SimFreeParam
# Class -- simsem package
# Set of vectors and matrices arrangements that will save free parameters and values of fixed parameters that will be used to model specification. 
# Constructor:	createFreeParameters(object)
# Parent Class: VirtualRSet
# Child Class:	None
# Methods:
#	findOpenMxValues
#	makeLabels
#	simModel
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("SimFreeParam", 
	contains="VirtualRSet"
)

###################################################################
# SimLabels
# Class -- simsem package
# Set of vectors and matrices arrangements that will save labels that will be used to run OpenMx. 
# Constructor:	makeLabels(SimFreeParam)
# Parent Class: VirtualRSet
# Child Class:	None
# Methods:
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("SimLabels", 
	contains="VirtualRSet"
)

###################################################################
# SimRSet
# Class -- simsem package
# Set of vectors and matrices arrangements that will save values that will be used for various purposes. 
# Constructor:	defaultStartingValues(object)
# Parent Class: VirtualRSet
# Child Class:	None
# Methods:
#	createImpliedMACS
#	findOpenMxValues
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("SimRSet", 
	contains="VirtualRSet"
)

###################################################################
# NullRSet
# Class -- simsem package
# Null object for VirtualRSet.c 
# Constructor:	new("NullRSet")
# Parent Class: VirtualRSet
# Child Class:	None
# Methods:
#	createImpliedMACS
#	findOpenMxValues
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011


setClass("NullRSet", 
	contains="SimRSet",
	representation(
		modelType="character",
		LY="matrix",
		TE="matrix",
		PS="matrix",
		BE="matrix",
		TY="vector",
		AL="vector",
		LX="matrix",
		TD="matrix",
		PH="matrix",
		GA="matrix",
		TX="vector",
		KA="vector",
		TH="matrix"),
	prototype(
		modelType=character(0),
		LY=new("NullMatrix"),
		TE=new("NullMatrix"),
		PS=new("NullMatrix"),
		BE=new("NullMatrix"),
		TY=new("NullVector"),
		AL=new("NullVector"),
		LX=new("NullMatrix"),
		TD=new("NullMatrix"),
		PH=new("NullMatrix"),
		GA=new("NullMatrix"),
		TX=new("NullVector"),
		KA=new("NullVector"),
		TH=new("NullMatrix"))
)

###################################################################
# SimMisspec
# Class -- simsem package
# Set of SimVector.c and SimMatrix.c that saves model misspecification (CFA, Path analysis, or SEM)
# Constructor:	
#	simMisspecCFA(...) for CFA
#	simMisspecPath(...) for Path analysis
#	simMisspecSEM(...)	for SEM
# Parent Class: SimSet
# Child Class:	NullSimMisspec
# Methods:
#	isNullObject
#	run
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("SimMisspec", 
	contains = "SimSet"
)

###################################################################
# NullSimMisspec
# Class -- simsem package
# The null object of SimMisspec.c
# Constructor:	new("SimMisspec")
# Parent Class: SimMisspec
# Child Class:	None
# Attributes:	SimMisspec with null objects in all attributes.
#		It does not matter because these attributes will not be used. 
#		It will be checked whether the class is NULL only.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("NullSimMisspec", contains = "SimMisspec")

###################################################################
# MisspecSet
# Class -- simsem package
# Set of vector.c and matrix.c that a random sample of SimMisspec.c
# Constructor:	run(SimMisspec)
# Parent Class: MatrixSet
# Child Class:	None
# Methods:
#	combineObject(list, MisspecSet)
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("MisspecSet", 
	contains = "MatrixSet"
)

###################################################################
# SimDataDist
# Class -- simsem package
# This class will save information for the multivariate distribution of a data
# Constructor:	simDataDist(...)
# Parent Class: None
# Child Class:	NullSimDataDist
# Attributes:
#	p:	Number of variables
#	dist:		List of distribution objects
# 	keepScale:	Keep mean and variance of the data (with sampling error)
#	reverse:	Reverse (mirror) the distribution (e.g., from right skewed to left skewed). This attribute is supposed to be a vector of logical values with the length of p.
# Methods:
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: February 24, 2011

setClass("SimDataDist", 
	representation(
		p="numeric",
		dist="list",
		keepScale="logical",
		reverse="vector"),
	prototype(
		keepScale=TRUE,
		reverse=FALSE)
)

###################################################################
# NullSimDataDist
# Class -- simsem package
# The null class of the SimDataDist
# Constructor:	new("NullSimDataDist")
# Parent Class: 	SimDataDist
# Child Class:	NONE
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: February 23, 2011

setClass("NullSimDataDist", contains="SimDataDist")


###################################################################
# SimData
# Class -- simsem package
# This class will save information for data simulation and can create data by run function
# Constructor:	simData(SimSet, N, SimMisspec=new("NullSimMisspec"), SimEqualCon=new("NullSimEqualCon"), conBeforeMis=TRUE, misfitBound=new("NullVector"), maxDraw=100)
# Parent Class: None
# Child Class:	None
# Attributes:
#	modelType:	Model type (CFA, Path, or SEM)
#	N:		Sample size 
#	param:		SimSet.c that save model specification
#	misspec:	SimMisspec.c that save model misspecification
#	equalCon:		SimEqualCon.c that specify equality constraint of parameters in data generation
#	Constrain.Parameter.Only:	TRUE if users wish to constrain parameters before adding misspecification. 
#								FALSE if users wish to constrain parameters after adding misspecification.
#	misfitBound:		max bound of population RMSEA that users wish their model misspecification to be
#	maxDraw:		The maximum number of random drawn parameters and misspecification model until all parameters in the model are eligible (no negative error variance, standardized coefficients over 1).
# Methods:
#	run
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("SimData", 
	representation(
		modelType="character",
		n="numeric",
		param="SimSet",
		misspec="SimMisspec",
		equalCon="SimEqualCon",
		conBeforeMis="logical",
		misfitBound="vector",
		maxDraw="numeric",
		sequential="logical",
		facDist="SimDataDist",
		errorDist="SimDataDist",
		indDist="SimDataDist",
		indicatorLab="vector"),
	prototype(
		misspec=new("NullSimMisspec"),
		equalCon=new("NullSimEqualCon"),
		conBeforeMis=TRUE,
		misfitBound=new("NullVector"),
		maxDraw=100,
		sequential=FALSE,
		facDist=new("NullSimDataDist"),
		errorDist=new("NullSimDataDist"),
		indDist=new("NullSimDataDist"),
		indicatorLab=new("NullVector"))
)

###################################################################
# SimDataOut
# Class -- simsem package
# This class will save information for data simulation and can create data by run function
# Constructor:	run(simData, n)
# Parent Class: None
# Child Class:	None
# Attributes:
#	modelType:	Model type (CFA, Path, or SEM)
#	data:	data.frame.c of a simulated data
#	param:		SimSet.c that save model specification
#	paramOut:	SimRSet.c that saves parameters values used in data generation
# 	misspecOut: SimRSet.c that saves model misspecification values used in data generation
#	equalCon:		SimEqualCon.c that specify equality constraint of parameters in data generation
# Methods:
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: November 16, 2011

setClass("SimDataOut", 
	representation(
		modelType="character",
		data="data.frame",
		param="SimFreeParam",
		#paramExp="SimRSet",
		#misspecExp="SimRSet",
		paramOut="SimRSet",
		misspecOut="SimRSet",
		equalCon="SimEqualCon",
		n="numeric"),
	prototype(
		equalCon=new("NullSimEqualCon"),
		n=0)
)

###################################################################
# SimModel
# Class -- simsem package
# This class will save information for analysis model and be ready for data analysis.
# Constructor:	simModel(SimSet, equalCon, package, trial)
#				simModel(SimFreeParam, start, equalCon, package)
# Parent Class: None
# Child Class:	None
# Attributes:
#	modelType:			Model type (CFA, Path, or SEM)
#	param:		SimFreeParam.c that save all free parameters and values of fixed parameters
#	start:	All starting values of free parameters in SimRSet.c
#	equalCon:		SimEqualCon.c that specify equality constraint of parameters in data analysis
#	package:		Packages used in the analysis (lavaan or OpenMx)
# Methods:
#	run
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("SimModel", 
	representation(
		modelType="character",
		param="SimFreeParam",
		start="SimRSet",
		equalCon="SimEqualCon",
		package="character", #OpenMx, lavaan
		estimator="character",
		auxiliary="vector",
		indicatorLab="vector", #X then Y
		factorLab="vector"), # K then E
	prototype(
		equalCon=new("NullSimEqualCon"),
		package="lavaan",
		estimator="ml",
		auxiliary=new("NullVector"),
		indicatorLab=new("NullVector"),
		factorLab=new("NullVector"))
)

###################################################################
# NullDataFrame
# Class -- simsem package
# The null object of data.frame.c
# Constructor:	new("NullDataFrame")
# Parent Class: data.frame
# Child Class:	None
# Attributes:	null data.frame 
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: November 16, 2011

setClass("NullDataFrame", 
	contains = "data.frame"
)

###################################################################
# SimResult
# Class -- simsem package
# This class will save data analysis results from multiple replications and ready to find some useful statistics, such as fit indices cutoffs or power.
# Constructor:	simResult(SimData, SimModel, NRep, seed = 123321, silent=FALSE)
# Parent Class: None
# Child Class:	None
# Attributes:
#	modelType:			Model type (CFA, Path, or SEM)
#	nRep:	Number of replication
#	coef:	data.frame.c of parameter estimates of each replication
#	se:			data.frame.c of standard error of each replication
#	fit:			data.frame.c of fit indices of each replication
#	converged:	Number of convergence replications
#	paramValue:	Parameter values of each replication
#	seed:		Random number seed
# Methods:	
#	summary
#	getCutoff
#	getPower
#	plotCutoff
#	plotPower
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: November 16, 2011

setClass("SimResult", 
	representation(
		modelType="character",
		nRep="numeric",
		coef="data.frame",
		se="data.frame",
		fit="data.frame",
		converged="vector",
		paramValue="data.frame",
		FMI1="data.frame",
		FMI2="data.frame",
		stdCoef="data.frame",
		seed="numeric",
		n="vector",
		pmMCAR="vector",
		pmMAR="vector"),
	prototype(
		stdCoef=new("NullDataFrame"),
		paramValue=new("NullDataFrame"),
		FMI1=new("NullDataFrame"),
		FMI2=new("NullDataFrame"),
		n=0,
		pmMCAR=0,
		pmMAR=0)
)

###################################################################
# SimModelOut
# Class -- simsem package
# This class will save information of the result of data analysis.
# Constructor:	 	run(SimModel, data)
# Parent Class: None
# Child Class:	None
# Attributes:
#	param:		SimFreeParam.c that save all free parameters and values of fixed parameters
#	start:	All starting values of free parameters in SimRSet.c
#	equalCon:		SimEqualCon.c that specify equality constraint of parameters in data analysis
#	package:		Packages used in the analysis (lavaan or OpenMx)
#	coef:	List of parameter estimates
#	se:			Standard errors of parameter estimates
#	fit:			Vector of fit indices
#	converged: 	TRUE if the analysis converge
#	paramValue:		Parameter values behind the data and model result
# Methods:	None for now.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: November 16, 2011


setClass("SimModelOut", # The class provides model result.
    representation(
        param="SimFreeParam",
        start="SimRSet",
        equalCon="SimEqualCon",
        package="character",
        coef="SimRSet",
        fit="vector",
        se="SimRSet",
        converged="logical",
		paramValue="SimRSet",
		n="numeric",
		indicatorLab="vector",
		factorLab="vector"),
    prototype(
        equalCon=new("NullSimEqualCon"),
        converged=FALSE,
		paramValue=new("NullRSet"),
		n=0,
		indicatorLab=new("NullVector"),
		factorLab=new("NullVector"))
)

###################################################################
# SimModelMIOut
# Class -- simsem package
# This class will save information of the result of data analysis after pooling of MI results.
# Constructor:	 	run(SimModel, data)
# Parent Class: SimModelOut
# Child Class:	None
# Attributes:
#	param:		SimFreeParam.c that save all free parameters and values of fixed parameters
#	start:	All starting values of free parameters in SimRSet.c
#	equalCon:		SimEqualCon.c that specify equality constraint of parameters in data analysis
#	package:		Packages used in the analysis (lavaan or OpenMx)
#	coef:	List of parameter estimates
#	se:			Standard errors of parameter estimates
#	fit:			Vector of fit indices
#	converged: 	TRUE if the analysis converge
#	paramValue:		Parameter values behind the data and model result
# Methods:	None for now.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: November 16, 2011


setClass("SimModelMIOut", # The class provides model result.
	contains="SimModelOut",
    representation(
        FMI1="SimRSet",
        FMI2="SimRSet"),
	prototype(
		FMI1=new("NullRSet"),
		FMI2=new("NullRSet"))
)

###################################################################
# SimMissing
# Class -- simsem package
# This class is a container for instructions to impose missing data for a simulation study. To be used with with a call to simResult.
# Constructor:	 	new(SimModel, data)
# Parent Class: None
# Child Class:	None
# Attributes:
#  covs = list of covariates included in the data set (data will not be removed from the covariates). Needed to impose MAR
#  pmMCAR = percent missing MCAR
#  pmMAR = percent missing MAR
#  nforms = number of forms for planned missing
#  itemGroups = list of lists of item groupings (column indices) for planned missing data designs
#  twoMethod = vector of (item, percent missing). Functions removes the given percent of data on the column index.
#  timepoints = number of timepoints in data
# Methods:	None for now.
# Author: Alex Schoemann (University of Kansas; schoemann@ku.edu), Patrick Miller (University of Kansas; patr1ckm@ku.edu)
# Date Modified: November 17, 2011

setClass("SimMissing",
    representation(
        cov="vector",
        pmMCAR="numeric",
        pmMAR="numeric",
        nforms="numeric",
        itemGroups="list",
        twoMethod="vector",
        impMethod="vector",
        numImps="numeric",
        timePoints="numeric",
		ignoreCols="numeric",
		threshold="numeric"),
    prototype(
        cov=0,
        pmMCAR=0,
        pmMAR=0,
        nforms=0,
        itemGroups=list(0),
        twoMethod=0,
        impMethod="amelia",
        numImps=0,
        timePoints=1,
		ignoreCols=0,
		threshold=0)
)


###################################################################
# NullSimMissing
# Class -- simsem package
# This null class of the SimMissing class
# Constructor:	 	new("NullSimModel", data)
# Parent Class: SimMissing
# Child Class:	None
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu), Patrick Miller (University of Kansas; patr1ckm@ku.edu)
# Date Modified: February 09, 2012

setClass("NullSimMissing", 
	contains="SimMissing"
)

#####################################################################
# SimFunction
# Class -- simsem package
# This class will save function so that the function will not run right now and will run in the appropriate place in the future
# Constructor:	simFunction(fun, ...)
# Parent Class:	None
# Child Class: NullSimFunction
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: April 10, 2012

setClass("SimFunction",
	representation(
		fun="function",
		attribute="list",
		callfun="call"
	)
)

###################################################################
# NullSimFunction
# Class -- simsem package
# This null class of the SimFunction class
# Constructor:	 	new("NullSimFunction", data)
# Parent Class: SimFunction
# Child Class:	None
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: April 10, 2012

setClass("NullSimFunction", 
	contains="SimFunction"
)
