simData <- function(N, SimSet, SimMisspec=new("NullSimMisspec"), SimEqualCon=new("NullSimEqualCon"), Constrain.Parameters.Only=TRUE, Misfit.bound=new("NullVector"), Maximum.random=100) {
	modelType <- SimSet@modelType
	#browser()
	if(!is.null.object(SimMisspec)) {
		if(modelType != SimMisspec@modelType) stop("SimMisspec and SimSet do not have the same tag")
	}
	if(!is.null.object(SimEqualCon)) {
		if(modelType != SimEqualCon@modelType) stop("SimEqualCon and SimSet do not have the same tag")
	}
	if(!is.null.object(Misfit.bound)) {
		if(length(Misfit.bound) == 2) {
			if(Misfit.bound[1] >= Misfit.bound[2]) stop("The lower bound is higher than the upper bound")
		} else {
			stop("Misfit.bound must include only two numbers for lower and upper bound")
		}
	}
	return(new("SimData", N=N, modelType=modelType, Parameters=SimSet, Misspecified=SimMisspec,
		Constraint=SimEqualCon, Constrain.Parameters.Only=Constrain.Parameters.Only, Misfit.bound=Misfit.bound, Maximum.random=Maximum.random))
}
