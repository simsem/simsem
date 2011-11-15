###################################################################
# SimUnif
# Class -- simsem package
# Object that create a random number from uniform distribution.
# Constructor:	simUnif(min, max)
# Parent Class: VirtualDist
# Child Class:	None
# Attributes:
#	min: 	min bound parameter
# 	max: 	max bound parameter
# Methods:
#	run
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("SimUnif",
	representation(
		min="numeric",
		max="numeric"
	)
)
#Examples:
#showClass("SimUnif")
#u1 <- simUnif(-0.1, 0.1)
#run(u1)
#summary(u1)

###################################################################
# SimNorm
# Class -- simsem package
# Object that create a random number from normal distribution.
# Constructor:	simNorm(M, sd)
# Parent Class: VirtualDist
# Child Class:	None
# Attributes:
#	M: 		The population mean of the normal distribution
# 	sd: 	The population standard deviation of the normal distribution
# Methods:
#	run
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("SimNorm",
	representation(
		mean="numeric",
		sd="numeric"
	)
)
#Examples:
#showClass("SimNorm")
#n2 <- simNorm(0, 0.2)
#run(n2)
#summary(n2)

###################################################################
# VirtualDist
# Class -- simsem package
# Virtual class that aggregates all distribution objects together.
# Constructor:	None
# Parent Class: None
# Child Class: SimNorm, SimUnif
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClassUnion("VirtualDist", c("SimUnif", "SimNorm"))

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
#	combine.object
#	count.random.object
#	is.null.object
#	run
#	starting.values
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
#	count.random.object
#	is.null.object
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
#PH <- symMatrix(latent.cor, 0.5)
#u46 <- simUnif(0.4, 0.6)
#PH <- adjust(PH, "u46", c(3,2))
#summary(PH)
#summaryShort(PH)
#run(PH)

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
#	combine.object
#	count.random.object
#	is.null.object
#	run
#	starting.values
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
#	TE:		SymMatrix.c of Correlation matrix between Y measurement error 
#	VTE:	SimVector.c of Variance of Y measurement error 
#	PS:		SymMatrix.c of Residual correlation of endogenous factors  
#	VPS:	SimVector.c of Residual variances of endogenous factors 
#	BE:		SimMatrix.c of Regression effect among endogenous factors 
#	TY:		SimVector.c of Measurement intercepts of Y indicators 
#	AL:		SimVector.c of Factor intercepts of endogenous factors 
#	ME:		SimVector.c of Factor means of endogenous factors 
#	MY:		SimVector.c of Total mean of Y indicators 
#	VE:		SimVector.c of Total variance of endogenous factors 
#	VY:		SimVector.c of Total variance of Y indicators 
#	LX:		SimMatrix.c of Factor loading matrix between exogenous factors and X indicators 
#	TD:		SymMatrix.c of Correlation matrix between X measurement error 
#	VTD:	SimVector.c of Variance of X measurement error 
#	PH:		SymMatrix.c of Correlation among exogenous factors 
#	GA:		SimMatrix.c of Regreeion effect from exogenous factors to endogenous factors 
#	TX:		SimVector.c of Measurement intercepts of X indicators 
#	KA:		SimVector.c of Factor mean of exogenous factors 
#	MX:		SimVector.c of Total mean of X indicators 
#	VPH:	SimVector.c of Variance of exogenous factors 
#	VX:		SimVector.c of Total variance of X indicators 
#	TH:		SimMatrix.c Measurement error correlation between X indicators and Y indicators 
# Methods:
#	count.random.object
#	is.null.object
#	simModel
#	run
#	starting.values
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("SimSet", 
	representation(
		modelType="character", #Path, Path.exo, CFA, SEM, SEM.exo
		LY="SimMatrix",
		TE="SymMatrix",
		VTE="SimVector",
		PS="SymMatrix",
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
		VTD="SimVector",
		PH="SymMatrix",
		GA="SimMatrix",
		TX="SimVector",
		KA="SimVector",
		MX="SimVector",
		VPH="SimVector",
		VX="SimVector",
		TH="SimMatrix"), #Delta on rows, epsilon on columns
	prototype(
		LY=new("NullSimMatrix"),
		TE=new("NullSymMatrix"),
		VTE=new("NullSimVector"),
		PS=new("NullSymMatrix"),
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
		VTD=new("NullSimVector"),
		PH=new("NullSymMatrix"),
		GA=new("NullSimMatrix"),
		TX=new("NullSimVector"),
		KA=new("NullSimVector"),
		MX=new("NullSimVector"),
		VPH=new("NullSimVector"),
		VX=new("NullSimVector"),
		TH=new("NullSimMatrix"))
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
#PH <- symMatrix(latent.cor, 0.5)
#error.cor <- matrix(0, 6, 6)
#diag(error.cor) <- 1
#TD <- symMatrix(error.cor)
#CFA.Model <- simSetCFA(LX = LX, PH = PH, TD = TD)
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
# Child Class:	misspecifiedSet
# Attributes:
#	modelType:	Model type (CFA, Path, or SEM)
#	LY:		matrix.c of Factor loading matrix between endogenous factors and Y indicators 
#	TE:		matrix.c of Correlation matrix between Y measurement error 
#	VTE:	vector.c of Variance of Y measurement error 
#	PS:		matrix.c of Residual correlation of endogenous factors  
#	VPS:	vector.c of Residual variances of endogenous factors 
#	BE:		matrix.c of Regression effect among endogenous factors 
#	TY:		vector.c of Measurement intercepts of Y indicators 
#	AL:		vector.c of Factor intercepts of endogenous factors 
#	ME:		vector.c of Factor means of endogenous factors 
#	MY:		vector.c of Total mean of Y indicators 
#	VE:		vector.c of Total variance of endogenous factors 
#	VY:		vector.c of Total variance of Y indicators 
#	LX:		matrix.c of Factor loading matrix between exogenous factors and X indicators 
#	TD:		matrix.c of Correlation matrix between X measurement error 
#	VTD:	vector.c of Variance of X measurement error 
#	PH:		matrix.c of Correlation among exogenous factors 
#	GA:		matrix.c of Regreeion effect from exogenous factors to endogenous factors 
#	TX:		vector.c of Measurement intercepts of X indicators 
#	KA:		vector.c of Factor mean of exogenous factors 
#	MX:		vector.c of Total mean of X indicators 
#	VPH:	vector.c of Variance of exogenous factors 
#	VX:		vector.c of Total variance of X indicators 
#	TH:		matrix.c Measurement error correlation between X indicators and Y indicators 
# Methods:
#	combine.object
#	create.implied.MACS
#	divide.object
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("MatrixSet", 
	representation(
		modelType="character",
		LY="matrix",
		TE="matrix",
		VTE="vector",
		PS="matrix",
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
		VTD="vector",
		PH="matrix",
		GA="matrix",
		TX="vector",
		KA="vector",
		MX="vector",
		VPH="vector",
		VX="vector",
		TH="matrix"),
	prototype(
		LY=new("NullMatrix"),
		TE=new("NullMatrix"),
		VTE=new("NullVector"),
		PS=new("NullMatrix"),
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
		VTD=new("NullVector"),
		PH=new("NullMatrix"),
		GA=new("NullMatrix"),
		TX=new("NullVector"),
		KA=new("NullVector"),
		MX=new("NullVector"),
		VPH=new("NullVector"),
		VX=new("NullVector"),
		TH=new("NullMatrix"))
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
#PH <- symMatrix(latent.cor, 0.5)
#error.cor <- matrix(0, 6, 6)
#diag(error.cor) <- 1
#TD <- symMatrix(error.cor)
#CFA.Model <- simSetCFA(LX = LX, PH = PH, TD = TD)
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
#	constrain.matrices
#	tag.headers
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
#	constrain.matrices(list, SimEqualCon)
#	constrain.matrices(VirtualRSet, SimEqualCon)
#	is.null.object
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
# Constructor:	reduce.constraint(SimEqualCon)
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
#	constrain.matrices(VirtualRSet, SimREqualCon)
#	is.null.object
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
# Constructor:	create.free.parameters(object)
# Parent Class: VirtualRSet
# Child Class:	None
# Methods:
#	find.OpenMx.values
#	make.labels
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
# Constructor:	make.labels(SimFreeParam)
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
# Constructor:	default.starting.values(object)
# Parent Class: VirtualRSet
# Child Class:	None
# Methods:
#	create.implied.MACS
#	find.OpenMx.values
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("SimRSet", 
	contains="VirtualRSet"
)
############ Change list of matrices to SimRSet ##################
########### May be impossible!! Let's see.

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
#	is.null.object
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
# misspecifiedSet
# Class -- simsem package
# Set of vector.c and matrix.c that a random sample of SimMisspec.c
# Constructor:	run(SimMisspec)
# Parent Class: MatrixSet
# Child Class:	None
# Methods:
#	combine.object(list, misspecifiedSet)
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("misspecifiedSet", 
	contains = "MatrixSet"
)

###################################################################
# SimData
# Class -- simsem package
# This class will save information for data simulation and can create data by run function
# Constructor:	simData(N, SimSet, SimMisspec=new("NullSimMisspec"), SimEqualCon=new("NullSimEqualCon"), Constrain.Parameters.Only=TRUE, Misfit.bound=new("NullVector"), Maximum.random=100)
# Parent Class: None
# Child Class:	None
# Attributes:
#	modelType:	Model type (CFA, Path, or SEM)
#	N:		Sample size 
#	Parameters:		SimSet.c that save model specification
#	Misspecified:	SimMisspec.c that save model misspecification
#	Constraint:		SimEqualCon.c that specify equality constraint of parameters in data generation
#	Constrain.Parameter.Only:	TRUE if users wish to constrain parameters before adding misspecification. 
#								FALSE if users wish to constrain parameters after adding misspecification.
#	Misfit.bound:		max bound of population RMSEA that users wish their model misspecification to be
#	Maximum.random:		The maximum number of random drawn parameters and misspecification model until all parameters in the model are eligible (no negative error variance, standardized coefficients over 1).
# Methods:
#	run
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("SimData", 
	representation(
		modelType="character",
		N="numeric",
		Parameters="SimSet",
		Misspecified="SimMisspec",
		Constraint="SimEqualCon",
		Constrain.Parameters.Only="logical",
		Misfit.bound="vector",
		Maximum.random="numeric"),
	prototype(
		Misspecified=new("NullSimMisspec"),
		Constraint=new("NullSimEqualCon"),
		Constrain.Parameters.Only=TRUE,
		Misfit.bound=new("NullVector"),
		Maximum.random=100)
)

###################################################################
# SimModel
# Class -- simsem package
# This class will save information for analysis model and be ready for data analysis.
# Constructor:	simModel(SimSet, Constraint, Program, trial)
#				simModel(SimFreeParam, Starting.Values, Constraint, Program)
# Parent Class: None
# Child Class:	None
# Attributes:
#	modelType:			Model type (CFA, Path, or SEM)
#	Parameters:		SimFreeParam.c that save all free parameters and values of fixed parameters
#	Starting.Values:	All starting values of free parameters in SimRSet.c
#	Constraint:		SimEqualCon.c that specify equality constraint of parameters in data analysis
#	Program:		Packages used in the analysis (lavaan or OpenMx)
# Methods:
#	run
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("SimModel", 
	representation(
		modelType="character",
		Parameters="SimFreeParam",
		Starting.Values="SimRSet",
		Constraint="SimEqualCon",
		Program="character"), #OpenMx, lavaan
	prototype(
		Constraint=new("NullSimEqualCon"),
		Program="lavaan")
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
#	Replication:	Number of replication
#	Estimates:	data.frame.c of parameter estimates of each replication
#	SE:			data.frame.c of standard error of each replication
#	Fit:			data.frame.c of fit indices of each replication
#	Convergence:	Number of convergence replications
#	Seed:		Random number seed
# Methods:	
#	summary
#	getCutoff
#	getPower
#	plotCutoff
#	plotPower
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 11, 2011

setClass("SimResult", 
	representation(
		modelType="character",
		Replication="numeric",
		Estimates="data.frame",
		SE="data.frame",
		Fit="data.frame",
		Convergence="vector",
		Seed="numeric")
)


###################################################################
# SimModelOut
# Class -- simsem package
# This class will save information of the result of data analysis.
# Constructor:	 	run(SimModel, data)
# Parent Class: None
# Child Class:	None
# Attributes:
#	Parameters:		SimFreeParam.c that save all free parameters and values of fixed parameters
#	Starting.Values:	All starting values of free parameters in SimRSet.c
#	Constraint:		SimEqualCon.c that specify equality constraint of parameters in data analysis
#	Program:		Packages used in the analysis (lavaan or OpenMx)
#	Estimates:	List of parameter estimates
#	SE:			Standard errors of parameter estimates
#	Fit:			Vector of fit indices
#	Convergence: 	TRUE if the analysis converge
# Methods:	None for now.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 11, 2011


setClass("SimModelOut", # The class provides model result.
         representation(
                        Parameters="SimFreeParam",
                        Starting.Values="SimRSet",
                        Constraint="SimEqualCon",
                        Program="character",
                        Estimates="SimRSet",
                        Fit="vector",
                        SE="SimRSet",
                        Convergence="logical"),
         prototype(
                Constraint=new("NullSimEqualCon"),
                   Convergence=FALSE)
         )
