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



setMethod("simModel", signature(object="SimFreeParam"), definition=function(object, Starting.Values = NULL, Constraint=new("NullSimEqualCon"), Program="lavaan") {
	modelType <- object@modelType
	if(!is.null(Starting.Values)) {
		if(modelType != Starting.Values@modelType) stop("Starting Values and Parameters do not have the same tag")
	} else {
		Starting.Values <- default.starting.values(object)
	}
	if(!is.null.object(SimEqualCon)) {
		if(modelType != Constraint@modelType) stop("SimEqualCon and SimFreeParam do not have the same tag")
	}
	return(new("SimModel", modelType=modelType, Parameters=object, Starting.Values=Starting.Values, Constraint=Constraint, Program=Program))
})
#Arguments: 
#	object:	SimFreeParam.c that save the specification of free parameters and values of fixed parameters
#	Starting.Values:	Starting values for those free parameters. lavaan package will automatically create starting values before the analysis; however, starting values are required for OpenMx.
# 	Constraint:		simConstrint.c that save constraints specified by users. The default is no constraint.
#	Program:	Desired analysis package
#Description: 	This function will set up all slots needed for SimModel.c 
#Return: 	SimModel.c with specification from the function.

setMethod("simModel", signature(object="SimSet"), definition=function(object, Constraint=new("NullSimEqualCon"), Program="lavaan", trial=10) {
	#browser()
	Starting.Values <- starting.values(object, trial)
	Starting.Values <- reduce.matrices(Starting.Values)
	#browser()
	freeParameters <- create.free.parameters(object)
	modelType <- object@modelType
	if(!is.null.object(Constraint)) {
		if(modelType != Constraint@modelType) stop("SimEqualCon and SimSet do not have the same tag")
	}
	return(new("SimModel", modelType=modelType, Parameters=freeParameters, Starting.Values=Starting.Values, Constraint=Constraint, Program=Program))
})
#Arguments: 
#	object:	SimSet.c that save the specification of free parameters and values of fixed parameters, as well as parameter values.
# 	Constraint:		simConstrint.c that save constraints specified by users. The default is no constraint.
#	Program:	Desired analysis package
#Description: 	This function will create starting.values from parameter values. If the parameters are specified as distribution object,
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
