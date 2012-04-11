# simModel
# Methods -- simsem package
# Create simModel from model specification
# Description: 	This function will take model specification from SimSet.c that contains free parameters, starting values, and fixed values. 
#		It will transform the code to a specified SEM package and ready to analyze data.
# Generic Function: simModel(object, ...)
# Argument:
#	object: 	SimSet.c or SimFreeParam.c that provides model specification
# 	... : 		Other arguments, such as analysis package, SimEqualCon.c
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 6, 2011



setMethod("simModel", signature(object="SimFreeParam"), definition=function(object, start = NULL, equalCon=new("NullSimEqualCon"), package="lavaan", estimator="ML", auxiliary=new("NullVector"), indicatorLab=new("NullVector"), factorLab=new("NullVector")) {
	modelType <- object@modelType
	if(length(intersect(indicatorLab, auxiliary)) != 0) stop("There are the same variables in the analysis model and in the auxiliary variables list")
	if(!is.null(start)) {
		if(modelType != start@modelType) stop("Starting Values and Parameters do not have the same tag")
	} else {
		start <- defaultStartingValues(object)
	}
	if(!isNullObject(equalCon)) {
		if(modelType != equalCon@modelType) stop("SimEqualCon and SimFreeParam do not have the same tag")
	}
	estimator <- tolower(estimator)
	return(new("SimModel", modelType=modelType, param=object, start=start, equalCon=equalCon, package=package, estimator=estimator, auxiliary=auxiliary, indicatorLab=indicatorLab, factorLab=factorLab))
})
#Arguments: 
#	object:	SimFreeParam.c that save the specification of free parameters and values of fixed parameters
#	start:	Starting values for those free parameters. lavaan package will automatically create starting values before the analysis; however, starting values are required for OpenMx.
# 	equalCon:		simConstrint.c that save constraints specified by users. The default is no constraint.
#	package:	Desired analysis package
# 	estimator:	Method of estimation
#Description: 	This function will set up all slots needed for SimModel.c 
#Return: 	SimModel.c with specification from the function.

setMethod("simModel", signature(object="SimSet"), definition=function(object, equalCon=new("NullSimEqualCon"), package="lavaan", trial=10, estimator="ML", auxiliary=new("NullVector"), indicatorLab=new("NullVector"), factorLab=new("NullVector")) {
	if(length(intersect(indicatorLab, auxiliary)) != 0) stop("There are the same variables in the analysis model and in the auxiliary variables list")
	start <- startingValues(object, trial)
	start <- reduceMatrices(start)
	freeParameters <- createFreeParameters(object)
	modelType <- object@modelType
	if(!isNullObject(equalCon)) {
		if(modelType != equalCon@modelType) stop("SimEqualCon and SimSet do not have the same tag")
	}
	estimator <- tolower(estimator)
	return(new("SimModel", modelType=modelType, param=freeParameters, start=start, equalCon=equalCon, package=package, estimator=estimator, auxiliary=auxiliary, indicatorLab=indicatorLab, factorLab=factorLab))
})
#Arguments: 
#	object:	SimSet.c that save the specification of free parameters and values of fixed parameters, as well as parameter values.
# 	equalCon:		simEqualCon.c that save constraints specified by users. The default is no constraint.
#	package:	Desired analysis package
# 	estimator: 	Method of estimation
#Description: 	This function will create startingValues from parameter values. If the parameters are specified as distribution object,
#		the model will find the average of 10 samples as starting values. Then, set up all slots needed for SimModel.c 
#Return: 	SimModel.c with specification from the function.

#Example:
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
#SimModel <- simModel(CFA.Model)

setMethod("simModel", signature(object="SimModelOut"), definition=function(object, start = NULL, equalCon=new("NullSimEqualCon"), package="lavaan", estimator="ML", auxiliary=new("NullVector"), indicatorLab=new("NullVector"), factorLab=new("NullVector")) {
	param <- object@param
	modelType <- param@modelType
	if(is.null(start)) start <- object@coef
	if(isNullObject(equalCon)) equalCon <- object@equalCon
	if(isNullObject(indicatorLab)) indicatorLab <- object@indicatorLab
	if(isNullObject(factorLab)) factorLab <- object@factorLab
	return(simModel(object=param, start=start, equalCon=equalCon, package=package, estimator=estimator, auxiliary=auxiliary, indicatorLab=indicatorLab, factorLab=factorLab)) 
})
#Arguments: 
#	object:	SimFreeParam.c that save the specification of free parameters and values of fixed parameters
#	start:	Starting values for those free parameters. lavaan package will automatically create starting values before the analysis; however, starting values are required for OpenMx.
# 	equalCon:		simConstrint.c that save constraints specified by users. The default is no constraint.
#	package:	Desired analysis package
# 	estimator:	Method of estimation
#Description: 	This function will set up all slots needed for SimModel.c 
#Return: 	SimModel.c with specification from the function.
