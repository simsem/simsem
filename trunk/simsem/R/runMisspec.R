# runMisspec
# function -- simsem package
# Run parameters from SimSet object with the parameters from SimMisspec to be put on top of it. The final parameters will be with and without model misspecification.
# Argument:
#	object: 	SimSet object of the real parameters
# 	misspec: 	Model misspecification object
#	SimEqualCon: 		Equality constraints
#	conBeforeMis:	Put model constraint before putting misspecification
# Return:	A list of
#		param:	SimRSet of real model parameters
#		misspec:	SimRSet of real model parameters with model misspecification
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)

runMisspec <- function(object, misspec, SimEqualCon=new("NullSimEqualCon"), conBeforeMis=TRUE) {
	if(conBeforeMis) {
		paramSet <- run(object, SimEqualCon, makeList=TRUE)
	} else {
		paramSet <- run(object, makeList=TRUE)
	}
	Output1 <- paramSet[[1]]
	Mis <- run(misspec)
	param <- combineObject(paramSet[[2]], Mis)
	if(!isNullObject(SimEqualCon) & (conBeforeMis=FALSE)) {
		if(object@modelType != SimEqualCon@modelType) stop("Please provide same tags of SimSet and constraint")
		param <- constrainMatrices(param, SimEqualCon)
	}
	Output2 <- fillParam(param, object@modelType)
	return(list(param=Output1, misspec=Output2))
}
