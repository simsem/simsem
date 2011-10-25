###################################################################
# Runif
# Class -- simsem package
# Object that create a random number from uniform distribution.
# Constructor:	runif.object(Lower, Upper)
# Parent Class: simDist
# Child Class:	None
# Attributes:
#	Lower: 	Lower bound parameter
# 	Upper: 	Upper bound parameter
# Methods:
#	run
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("Runif",
	representation(
		Lower="numeric",
		Upper="numeric"
	)
)
#Examples:
#showClass("Runif")
#u1 <- runif.object(-0.1, 0.1)
#run(u1)
#summary(u1)

###################################################################
# Rnorm
# Class -- simsem package
# Object that create a random number from normal distribution.
# Constructor:	rnorm.object(M, SD)
# Parent Class: simDist
# Child Class:	None
# Attributes:
#	M: 		The population mean of the normal distribution
# 	SD: 	The population standard deviation of the normal distribution
# Methods:
#	run
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("Rnorm",
	representation(
		Mean="numeric",
		SD="numeric"
	)
)
#Examples:
#showClass("Rnorm")
#n2 <- rnorm.object(0, 0.2)
#run(n2)
#summary(n2)

###################################################################
# simDist
# Class -- simsem package
# Virtual class that aggregates all distribution objects together.
# Constructor:	None
# Parent Class: None
# Child Class: Rnorm, Runif
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClassUnion("simDist", c("Runif", "Rnorm"))

###################################################################
# simMatrix
# Class -- simsem package
# This object can be used to represent a matrix in SEM model. It contains free parameters, fixed values, and starting values. 
# This object can be represented factor loading matrix or regreesion coefficient matrix. 
# Constructor:	matrix.object(Matrix, name.dist.object=NULL)
# Parent Class: None
# Child Class:	symMatrix, nullSimMatrix, nullSymMatrix (2 generation)
# Attributes:
#	Data: 		Free parameters as NA or values of fixed parameters
# 	Labels: 	All population/starting values of those free parameters
# Methods:
#	adjust.object
#	combine.object
#	count.random.object
#	is.null.object
#	run
#	starting.values
#	summary.short
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("simMatrix", 
	representation(
		Data="matrix",
		Labels="matrix"
	), 
	prototype(Data=as.matrix(NaN), Labels=as.matrix(NaN))
)
#Examples:
#showClass("simMatrix")
#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#loadingValues[1:3, 1] <- 0.7
#loadingValues[4:6, 2] <- 0.7
#LX <- matrix.object(loading, loadingValues)
#summary(LX)
#run(LX)
#n65 <- rnorm.object(0.6, 0.05)
#LY <- matrix.object(loading, "n65")
#summary(LY)
#run(LY)
#u34 <- runif.object(0.3, 0.4)
#LY <- adjust.object(LY, "u34", c(2, 1))
#summary(LY)
#run(LY)
#summary.short(LY)

###################################################################
# symMatrix
# Class -- simsem package
# This object can be used to represent a symmetric matrix in SEM model. It contains free parameters, fixed values, and starting values. 
# This object can be represented factor correlation or error correlation matrix.
# Constructor:	sym.matrix.object(Matrix, name.dist.object=NULL)
# Parent Class: None
# Child Class:	nullSymMatrix
# Attributes:
#	Data: 		Free parameters as NA or values of fixed parameters
# 	Labels: 	All population/starting values of those free parameters
# Methods:
#	adjust.object
#	count.random.object
#	is.null.object
#	run
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("symMatrix",
	contains = "simMatrix"
)
#Examples:
#showClass("symMatrix")
#latent.cor <- matrix(NA, 3, 3)
#diag(latent.cor) <- 1
#PH <- sym.matrix.object(latent.cor, 0.5)
#u46 <- runif.object(0.4, 0.6)
#PH <- adjust.object(PH, "u46", c(3,2))
#summary(PH)
#summary.short(PH)
#run(PH)

###################################################################
# simVector
# Class -- simsem package
# This object can be used to represent a vector in SEM model. It contains free parameters, fixed values, and starting values. 
# This object can be represented mean, intercept, or variance vectors.
# Constructor:	vector.object(Matrix, name.dist.object=NULL)
# Parent Class: None
# Child Class:	nullSimVector
# Attributes:
#	Data: 		Free parameters as NA or values of fixed parameters
# 	Labels: 	All population/starting values of those free parameters
# Methods:
#	adjust.object
#	combine.object
#	count.random.object
#	is.null.object
#	run
#	starting.values
#	summary.short
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("simVector", 
	representation(
		Data="vector",
		Labels="vector"
	), 
	prototype(Data=as.vector(NaN), Labels=as.vector(NaN))
)
#Examples:
#showClass("simVector")
#factor.mean <- rep(NA, 2)
#factor.mean.starting <- c(5, 2)
#AL <- vector.object(factor.mean, factor.mean.starting)
#run(AL)
#summary(AL)
#summary.short(AL)
#n01 <- rnorm.object(0, 1)
#AL <- adjust.object(AL, "n01", 2)
#run(AL)
#summary(AL)

###################################################################
# nullVector
# Class -- simsem package
# The null object of vector.c
# Constructor:	new("nullVector")
# Parent Class: vector
# Child Class:	None
# Attributes:	Zero length vector
#		Does not matter because these attributes will not be used. 
#		It will be checked whether the class is NULL only.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("nullVector", contains = "vector")

###################################################################
# nullMatrix
# Class -- simsem package
# The null object of matrix.c
# Constructor:	new("nullMatrix")
# Parent Class: matrix
# Child Class:	None
# Attributes:	Matrix with 0 x 0 dimension
#		Does not matter because these attributes will not be used. 
#		It will be checked whether the class is NULL only.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("nullMatrix", contains = "matrix")

###################################################################
# nullSimMatrix
# Class -- simsem package
# The null object of simMatrix.c
# Constructor:	new("nullSimMatrix")
# Parent Class: simMatrix
# Child Class:	None
# Attributes:	simMatrix with NaN in both attributes
#		Does not matter because these attributes will not be used. 
#		It will be checked whether the class is NULL only.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("nullSimMatrix", contains="simMatrix")

###################################################################
# nullSymMatrix
# Class -- simsem package
# The null object of symMatrix.c
# Constructor:	new("nullSymMatrix")
# Parent Class: symMatrix
# Child Class:	None
# Attributes:	symMatrix with NaN in both attributes
#		Does not matter because these attributes will not be used. 
#		It will be checked whether the class is NULL only.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("nullSymMatrix", contains="symMatrix")

###################################################################
# nullSimVector
# Class -- simsem package
# The null object of simVector.c
# Constructor:	new("nullSimVector")
# Parent Class: simVector
# Child Class:	None
# Attributes:	simVector with NaN in both attributes
#		Does not matter because these attributes will not be used. 
#		It will be checked whether the class is NULL only.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("nullSimVector", contains="simVector")

###################################################################
# simMatrixSet
# Class -- simsem package
# Set of simVector.c and simMatrix.c that saves model specification (CFA, Path analysis, or SEM)
# Constructor:	
#	matrix.CFA.object(...) for CFA
#	matrix.Path.object(...) for Path analysis
#	matrix.SEM.object(...)	for SEM
# Parent Class: None
# Child Class:	nullSimMatrixSet, simMisspecifiedSet, nullSimMisspecifiedSet (2 generations)
# Attributes:
#	Tag:	Model type (CFA, Path, or SEM)
#	LY:		simMatrix.c of Factor loading matrix between endogenous factors and Y indicators 
#	TE:		symMatrix.c of Correlation matrix between Y measurement error 
#	VTE:	simVector.c of Variance of Y measurement error 
#	PS:		symMatrix.c of Residual correlation of endogenous factors  
#	VPS:	simVector.c of Residual variances of endogenous factors 
#	BE:		simMatrix.c of Regression effect among endogenous factors 
#	TY:		simVector.c of Measurement intercepts of Y indicators 
#	AL:		simVector.c of Factor intercepts of endogenous factors 
#	ME:		simVector.c of Factor means of endogenous factors 
#	MY:		simVector.c of Total Mean of Y indicators 
#	VE:		simVector.c of Total variance of endogenous factors 
#	VY:		simVector.c of Total variance of Y indicators 
#	LX:		simMatrix.c of Factor loading matrix between exogenous factors and X indicators 
#	TD:		symMatrix.c of Correlation matrix between X measurement error 
#	VTD:	simVector.c of Variance of X measurement error 
#	PH:		symMatrix.c of Correlation among exogenous factors 
#	GA:		simMatrix.c of Regreeion effect from exogenous factors to endogenous factors 
#	TX:		simVector.c of Measurement intercepts of X indicators 
#	KA:		simVector.c of Factor Mean of exogenous factors 
#	MX:		simVector.c of Total Mean of X indicators 
#	VPH:	simVector.c of Variance of exogenous factors 
#	VX:		simVector.c of Total variance of X indicators 
#	TH:		simMatrix.c Measurement error correlation between X indicators and Y indicators 
# Methods:
#	count.random.object
#	is.null.object
#	model.object
#	run
#	starting.values
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("simMatrixSet", 
	representation(
		Tag="character", #Path, Path.exo, CFA, SEM, SEM.exo
		LY="simMatrix",
		TE="symMatrix",
		VTE="simVector",
		PS="symMatrix",
		VPS="simVector",
		BE="simMatrix",
		TY="simVector",
		AL="simVector",
		ME="simVector",
		MY="simVector",
		VE="simVector",
		VY="simVector",
		LX="simMatrix",
		TD="symMatrix",
		VTD="simVector",
		PH="symMatrix",
		GA="simMatrix",
		TX="simVector",
		KA="simVector",
		MX="simVector",
		VPH="simVector",
		VX="simVector",
		TH="simMatrix"), #Delta on rows, epsilon on columns
	prototype(
		LY=new("nullSimMatrix"),
		TE=new("nullSymMatrix"),
		VTE=new("nullSimVector"),
		PS=new("nullSymMatrix"),
		VPS=new("nullSimVector"),
		BE=new("nullSimMatrix"),
		TY=new("nullSimVector"),
		AL=new("nullSimVector"),
		ME=new("nullSimVector"),
		MY=new("nullSimVector"),
		VE=new("nullSimVector"),
		VY=new("nullSimVector"), 
		LX=new("nullSimMatrix"),
		TD=new("nullSymMatrix"),
		VTD=new("nullSimVector"),
		PH=new("nullSymMatrix"),
		GA=new("nullSimMatrix"),
		TX=new("nullSimVector"),
		KA=new("nullSimVector"),
		MX=new("nullSimVector"),
		VPH=new("nullSimVector"),
		VX=new("nullSimVector"),
		TH=new("nullSimMatrix"))
)
#Examples:
#showClass("simMatrixSet")
#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#loadingValues[1:3, 1] <- 0.7
#loadingValues[4:6, 2] <- 0.7
#LX <- matrix.object(loading, loadingValues)
#summary(LX)
#latent.cor <- matrix(NA, 2, 2)
#diag(latent.cor) <- 1
#PH <- sym.matrix.object(latent.cor, 0.5)
#error.cor <- matrix(0, 6, 6)
#diag(error.cor) <- 1
#TD <- sym.matrix.object(error.cor)
#CFA.Model <- matrix.CFA.object(LX = LX, PH = PH, TD = TD)
#summary(CFA.Model)
#run(CFA.Model)

###################################################################
# nullSimMatrixSet
# Class -- simsem package
# The null object of simMatrixSet.c
# Constructor:	new("nullSimMatrixSet")
# Parent Class: simMatrixSet
# Child Class:	None
# Attributes:	simMatrixSet with null object in all attributes.
#		It does not matter because these attributes will not be used. 
#		It will be checked whether the class is NULL only.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("nullSimMatrixSet", contains="simMatrixSet")

###################################################################
# matrixSet
# Class -- simsem package
# Set of vectors and matrices that saves model specification (CFA, Path analysis, or SEM). 
# This class is the result of running simMatrixSet.c. This will be a sample of all parameters of distribution objects.
# Constructor:	run(simMatrixSet)
# Parent Class: None
# Child Class:	misspecifiedSet
# Attributes:
#	Tag:	Model type (CFA, Path, or SEM)
#	LY:		matrix.c of Factor loading matrix between endogenous factors and Y indicators 
#	TE:		matrix.c of Correlation matrix between Y measurement error 
#	VTE:	vector.c of Variance of Y measurement error 
#	PS:		matrix.c of Residual correlation of endogenous factors  
#	VPS:	vector.c of Residual variances of endogenous factors 
#	BE:		matrix.c of Regression effect among endogenous factors 
#	TY:		vector.c of Measurement intercepts of Y indicators 
#	AL:		vector.c of Factor intercepts of endogenous factors 
#	ME:		vector.c of Factor means of endogenous factors 
#	MY:		vector.c of Total Mean of Y indicators 
#	VE:		vector.c of Total variance of endogenous factors 
#	VY:		vector.c of Total variance of Y indicators 
#	LX:		matrix.c of Factor loading matrix between exogenous factors and X indicators 
#	TD:		matrix.c of Correlation matrix between X measurement error 
#	VTD:	vector.c of Variance of X measurement error 
#	PH:		matrix.c of Correlation among exogenous factors 
#	GA:		matrix.c of Regreeion effect from exogenous factors to endogenous factors 
#	TX:		vector.c of Measurement intercepts of X indicators 
#	KA:		vector.c of Factor Mean of exogenous factors 
#	MX:		vector.c of Total Mean of X indicators 
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

setClass("matrixSet", 
	representation(
		Tag="character",
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
		LY=new("nullMatrix"),
		TE=new("nullMatrix"),
		VTE=new("nullVector"),
		PS=new("nullMatrix"),
		VPS=new("nullVector"),
		BE=new("nullMatrix"),
		TY=new("nullVector"),
		AL=new("nullVector"),
		ME=new("nullVector"),
		MY=new("nullVector"),
		VE=new("nullVector"),
		VY=new("nullVector"),
		LX=new("nullMatrix"),
		TD=new("nullMatrix"),
		VTD=new("nullVector"),
		PH=new("nullMatrix"),
		GA=new("nullMatrix"),
		TX=new("nullVector"),
		KA=new("nullVector"),
		MX=new("nullVector"),
		VPH=new("nullVector"),
		VX=new("nullVector"),
		TH=new("nullMatrix"))
)
#Examples:
#showClass("simMatrixSet")
#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#loadingValues[1:3, 1] <- 0.7
#loadingValues[4:6, 2] <- 0.7
#LX <- matrix.object(loading, loadingValues)
#latent.cor <- matrix(NA, 2, 2)
#diag(latent.cor) <- 1
#PH <- sym.matrix.object(latent.cor, 0.5)
#error.cor <- matrix(0, 6, 6)
#diag(error.cor) <- 1
#TD <- sym.matrix.object(error.cor)
#CFA.Model <- matrix.CFA.object(LX = LX, PH = PH, TD = TD)
#MatrixSet <- run(CFA.Model)
#summary(MatrixSet)

###################################################################
# blankReducedMatrixSet
# Class -- simsem package
# Set of vectors and matrices arrangements that will save free parameters, labels, or numbers on its child class. 
# This class will collapse those separation between mean and intercept into intercept only, as well as variance and correlation into covariance matrix only.
# Constructor:	Depends on its child class
# Parent Class: None
# Child Class:	freeParamSet, labelsSet, reducedMatrixSet
# Attributes:
#	Tag:	Model type (CFA, Path, or SEM)
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
#	KA:		vector.c of Factor Mean of exogenous factors 
#	TH:		matrix.c Measurement error covariance between X indicators and Y indicators 
# Methods:
#	constrain.matrices
#	tag.headers
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("blankReducedMatrixSet", 
	representation(
		Tag="character",
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
		LY=new("nullMatrix"),
		TE=new("nullMatrix"),
		PS=new("nullMatrix"),
		BE=new("nullMatrix"),
		TY=new("nullVector"),
		AL=new("nullVector"),
		LX=new("nullMatrix"),
		TD=new("nullMatrix"),
		PH=new("nullMatrix"),
		GA=new("nullMatrix"),
		TX=new("nullVector"),
		KA=new("nullVector"),
		TH=new("nullMatrix"))
)

###################################################################
# simConstraint
# Class -- simsem package
# Set of equality constraints that users wish to specify
# Constructor:	constraint.object(..., Tag)
# Parent Class: None
# Child Class:	nullSimConstrint
# Attributes:
#	Equality:	List of equality constraint. Each element in the list is an individual equality constraint saved in a matrix.
#			Each row represents each element. If the matrix has two columns, the first column indicates row of the element and 
#			the second column indicates column of the element. If the matrix has three columns, the first column is the group
#			of matrix. The rest is row and column. Row name represents the matrix that the element is in. The definition of row
#			name can be seen in matrix.CFA.object, matrix.Path.object, or matrix.SEM.object, depending on analysis model you specify.
#	Tag:	Analysis model (CFA, SEM, Path)
# Methods:
#	constrain.matrices(list, simConstraint)
#	constrain.matrices(blankReducedMatrixSet, simConstraint)
#	is.null.object
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("simConstraint", 
	representation(
		Equality="list",
		Tag="character")
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
#equal.loading <- constraint.object(constraint1, constraint2, constraint3, Tag="SEM")

###################################################################
# nullSimConstraint
# Class -- simsem package
# The null object of simConstraint.c
# Constructor:	new("nullSimConstraint")
# Parent Class: simConstriant
# Child Class:	None
# Attributes:	simConstraint with nothing in all attributes.
#		It does not matter because these attributes will not be used. 
#		It will be checked whether the class is NULL only.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("nullSimConstraint", contains="simConstraint", 
	representation(
		Equality="list",
		Tag="character"), 
	prototype(Equality=list(NA), Tag="NA")
)

###################################################################
# simReducedConstraint
# Class -- simsem package
# Set of equality constraints that users wish to specify
# Constructor:	reduce.constraint(simConstraint)
# Parent Class: None
# Child Class:	nullSimReducedConstraint
# Attributes:
#	Equality:	List of equality constraint. Each element in the list is an individual equality constraint saved in a matrix.
#			Each row represents each element. If the matrix has two columns, the first column indicates row of the element and 
#			the second column indicates column of the element. If the matrix has three columns, the first column is the group
#			of matrix. The rest is row and column. Row name represents the matrix that the element is in. The definition of row
#			name can be seen in blankReducedMatrixSet definition.
#	Tag:	Analysis model (CFA, SEM, Path)
# Methods:
#	constrain.matrices(blankReducedMatrixSet, simReducedConstraint)
#	is.null.object
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("simReducedConstraint", 
	representation(
		Equality="list",
		Tag="character")
)

###################################################################
# nullSimReducedConstraint
# Class -- simsem package
# The null object of simReducedConstraint.c
# Constructor:	new("nullSimReducedConstraint")
# Parent Class: simReducedConstriant
# Child Class:	None
# Attributes:	simReducedConstraint with nothing in all attributes.
#		It does not matter because these attributes will not be used. 
#		It will be checked whether the class is NULL only.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("nullSimReducedConstraint", contains="simReducedConstraint", 
	representation(
		Equality="list",
		Tag="character"), 
	prototype(Equality=list(NA), Tag="NA")
)

###################################################################
# freeParamSet
# Class -- simsem package
# Set of vectors and matrices arrangements that will save free parameters and values of fixed parameters that will be used to model specification. 
# Constructor:	create.free.parameters(object)
# Parent Class: blankReducedMatrixSet
# Child Class:	None
# Methods:
#	find.OpenMx.values
#	make.labels
#	model.object
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("freeParamSet", 
	contains="blankReducedMatrixSet"
)

###################################################################
# labelsSet
# Class -- simsem package
# Set of vectors and matrices arrangements that will save labels that will be used to run OpenMx. 
# Constructor:	make.labels(freeParamSet)
# Parent Class: blankReducedMatrixSet
# Child Class:	None
# Methods:
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("labelsSet", 
	contains="blankReducedMatrixSet"
)

###################################################################
# reducedMatrixSet
# Class -- simsem package
# Set of vectors and matrices arrangements that will save values that will be used for various purposes. 
# Constructor:	default.starting.values(object)
# Parent Class: blankReducedMatrixSet
# Child Class:	None
# Methods:
#	create.implied.MACS
#	find.OpenMx.values
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("reducedMatrixSet", 
	contains="blankReducedMatrixSet"
)
############ Change list of matrices to reducedMatrixSet ##################
########### May be impossible!! Let's see.

###################################################################
# simMisspecifiedSet
# Class -- simsem package
# Set of simVector.c and simMatrix.c that saves model misspecification (CFA, Path analysis, or SEM)
# Constructor:	
#	misspecified.CFA.object(...) for CFA
#	misspecified.Path.object(...) for Path analysis
#	misspecified.SEM.object(...)	for SEM
# Parent Class: simMatrixSet
# Child Class:	nullSimMisspecifiedSet
# Methods:
#	is.null.object
#	run
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("simMisspecifiedSet", 
	contains = "simMatrixSet"
)

###################################################################
# nullSimMisspecifiedSet
# Class -- simsem package
# The null object of simMisspecifiedSet.c
# Constructor:	new("simMisspecifiedSet")
# Parent Class: simMisspecifiedSet
# Child Class:	None
# Attributes:	simMisspecifiedSet with null objects in all attributes.
#		It does not matter because these attributes will not be used. 
#		It will be checked whether the class is NULL only.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("nullSimMisspecifiedSet", contains = "simMisspecifiedSet")

###################################################################
# misspecifiedSet
# Class -- simsem package
# Set of vector.c and matrix.c that a random sample of simMisspecifiedSet.c
# Constructor:	run(simMisspecifiedSet)
# Parent Class: matrixSet
# Child Class:	None
# Methods:
#	combine.object(list, misspecifiedSet)
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("misspecifiedSet", 
	contains = "matrixSet"
)

###################################################################
# simData
# Class -- simsem package
# This class will save information for data simulation and can create data by run function
# Constructor:	data.object(N, simMatrixSet, simMisspecifiedSet=new("nullSimMisspecifiedSet"), simConstraint=new("nullSimConstraint"), Constrain.Parameters.Only=TRUE, Misfit.bound=new("nullVector"), Maximum.random=100)
# Parent Class: None
# Child Class:	None
# Attributes:
#	Tag:	Model type (CFA, Path, or SEM)
#	N:		Sample size 
#	Parameters:		simMatrixSet.c that save model specification
#	Misspecified:	simMisspecifiedSet.c that save model misspecification
#	Constraint:		simConstraint.c that specify equality constraint of parameters in data generation
#	Constrain.Parameter.Only:	TRUE if users wish to constrain parameters before adding misspecification. 
#								FALSE if users wish to constrain parameters after adding misspecification.
#	Misfit.bound:		Upper bound of population RMSEA that users wish their model misspecification to be
#	Maximum.random:		The maximum number of random drawn parameters and misspecification model until all parameters in the model are eligible (no negative error variance, standardized coefficients over 1).
# Methods:
#	run
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("simData", 
	representation(
		Tag="character",
		N="numeric",
		Parameters="simMatrixSet",
		Misspecified="simMisspecifiedSet",
		Constraint="simConstraint",
		Constrain.Parameters.Only="logical",
		Misfit.bound="vector",
		Maximum.random="numeric"),
	prototype(
		Misspecified=new("nullSimMisspecifiedSet"),
		Constraint=new("nullSimConstraint"),
		Constrain.Parameters.Only=TRUE,
		Misfit.bound=new("nullVector"),
		Maximum.random=100)
)

###################################################################
# simModel
# Class -- simsem package
# This class will save information for analysis model and be ready for data analysis.
# Constructor:	model.object(simMatrixSet, Constraint, Program, trial)
#				model.object(freeParamSet, Starting.Values, Constraint, Program)
# Parent Class: None
# Child Class:	None
# Attributes:
#	Tag:			Model type (CFA, Path, or SEM)
#	Parameters:		freeParamSet.c that save all free parameters and values of fixed parameters
#	Starting.Values:	All starting values of free parameters in reducedMatrixSet.c
#	Constraint:		simConstraint.c that specify equality constraint of parameters in data analysis
#	Program:		Packages used in the analysis (lavaan or OpenMx)
# Methods:
#	run
#	summary
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 7, 2011

setClass("simModel", 
	representation(
		Tag="character",
		Parameters="freeParamSet",
		Starting.Values="reducedMatrixSet",
		Constraint="simConstraint",
		Program="character"), #OpenMx, lavaan
	prototype(
		Constraint=new("nullSimConstraint"),
		Program="lavaan")
)

###################################################################
# simResult
# Class -- simsem package
# This class will save data analysis results from multiple replications and ready to find some useful statistics, such as fit indices cutoffs or power.
# Constructor:	result.object(simData, simModel, NRep, seed = 123321, silent=FALSE)
# Parent Class: None
# Child Class:	None
# Attributes:
#	Tag:			Model type (CFA, Path, or SEM)
#	Replication:	Number of replication
#	Estimates:	data.frame.c of parameter estimates of each replication
#	SE:			data.frame.c of standard error of each replication
#	Fit:			data.frame.c of fit indices of each replication
#	Convergence:	Number of convergence replications
#	Seed:		Random number seed
# Methods:	
#	summary
#	find.cutoff
#	find.power
#	plot.cutoff
#	plot.power
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 11, 2011

setClass("simResult", 
	representation(
		Tag="character",
		Replication="numeric",
		Estimates="data.frame",
		SE="data.frame",
		Fit="data.frame",
		Convergence="vector",
		Seed="numeric")
)


###################################################################
# simAnalysis
# Class -- simsem package
# This class will save information of the result of data analysis.
# Constructor:	 	run(simModel, data)
# Parent Class: None
# Child Class:	None
# Attributes:
#	Parameters:		freeParamSet.c that save all free parameters and values of fixed parameters
#	Starting.Values:	All starting values of free parameters in reducedMatrixSet.c
#	Constraint:		simConstraint.c that specify equality constraint of parameters in data analysis
#	Program:		Packages used in the analysis (lavaan or OpenMx)
#	Estimates:	List of parameter estimates
#	SE:			Standard errors of parameter estimates
#	Fit:			Vector of fit indices
#	Convergence: 	TRUE if the analysis converge
# Methods:	None for now.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 11, 2011


setClass("simAnalysis", # The class provides model result.
         representation(
                        Parameters="freeParamSet",
                        Starting.Values="reducedMatrixSet",
                        Constraint="simConstraint",
                        Program="character",
                        Estimates="reducedMatrixSet",
                        Fit="vector",
                        SE="reducedMatrixSet",
                        Convergence="logical"),
         prototype(
                Constraint=new("nullSimConstraint"),
                   Convergence=FALSE)
         )
