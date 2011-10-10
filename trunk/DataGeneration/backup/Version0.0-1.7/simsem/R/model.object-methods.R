# model.object
# Methods -- simsem package
# Create model object from model specification
# Description: 	This function will take model specification from simMatrixSet.c that contains free parameters, starting values, and fixed values. 
#		It will transform the code to a specified SEM package and ready to analyze data.
# Generic Function: model.object(object, ...)
# Argument:
#	object: 	simMatrixSet.c or freeParamSet.c that provides model specification
# 	... : 		Other arguments, such as analysis package, simConstraint.c
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 6, 2011



setMethod("model.object", signature(object="freeParamSet"), definition=function(object, Starting.Values = NULL, Constraint=new("nullSimConstraint"), Program="lavaan") {
	Tag <- object@Tag
	if(!is.null(Starting.Values)) {
		if(Tag != Starting.Values@Tag) stop("Starting Values and Parameters do not have the same tag")
	} else {
		Starting.Values <- default.starting.values(object)
	}
	if(!is.null.object(simConstraint)) {
		if(Tag != Constraint@Tag) stop("simConstraint and freeParamSet do not have the same tag")
	}
	return(new("simModel", Tag=Tag, Parameters=object, Starting.Values=Starting.Values, Constraint=Constraint, Program=Program))
})
#Arguments: 
#	object:	freeParamSet.c that save the specification of free parameters and values of fixed parameters
#	Starting.Values:	Starting values for those free parameters. lavaan package will automatically create starting values before the analysis; however, starting values are required for OpenMx.
# 	Constraint:		simConstrint.c that save constraints specified by users. The default is no constraint.
#	Program:	Desired analysis package
#Description: 	This function will set up all slots needed for simModel.c 
#Return: 	simModel.c with specification from the function.

setMethod("model.object", signature(object="simMatrixSet"), definition=function(object, Constraint=new("nullSimConstraint"), Program="lavaan", trial=10) {
	#browser()
	Starting.Values <- starting.values(object, trial)
	Starting.Values <- reduce.matrices(Starting.Values)
	#browser()
	freeParameters <- create.free.parameters(object)
	Tag <- object@Tag
	if(!is.null.object(Constraint)) {
		if(Tag != Constraint@Tag) stop("simConstraint and simMatrixSet do not have the same tag")
	}
	return(new("simModel", Tag=Tag, Parameters=freeParameters, Starting.Values=Starting.Values, Constraint=Constraint, Program=Program))
})
#Arguments: 
#	object:	simMatrixSet.c that save the specification of free parameters and values of fixed parameters, as well as parameter values.
# 	Constraint:		simConstrint.c that save constraints specified by users. The default is no constraint.
#	Program:	Desired analysis package
#Description: 	This function will create starting.values from parameter values. If the parameters are specified as distribution object,
#		the model will find the average of 10 samples as starting values. Then, set up all slots needed for simModel.c 
#Return: 	simModel.c with specification from the function.

#Example:
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
#SimModel <- model.object(CFA.Model)
