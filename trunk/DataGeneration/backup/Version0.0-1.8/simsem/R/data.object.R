data.object <- function(N, simMatrixSet, simMisspecifiedSet=new("nullSimMisspecifiedSet"), simConstraint=new("nullSimConstraint"), Constrain.Parameters.Only=TRUE, Misfit.bound=new("nullVector"), Maximum.random=100) {
	Tag <- simMatrixSet@Tag
	#browser()
	if(!is.null.object(simMisspecifiedSet)) {
		if(Tag != simMisspecifiedSet@Tag) stop("simMisspecifiedSet and simMatrixSet do not have the same tag")
	}
	if(!is.null.object(simConstraint)) {
		if(Tag != simConstraint@Tag) stop("simConstraint and simMatrixSet do not have the same tag")
	}
	if(!is.null.object(Misfit.bound)) {
		if(length(Misfit.bound) == 2) {
			if(Misfit.bound[1] >= Misfit.bound[2]) stop("The lower bound is higher than the upper bound")
		} else {
			stop("Misfit.bound must include only two numbers for lower and upper bound")
		}
	}
	return(new("simData", N=N, Tag=Tag, Parameters=simMatrixSet, Misspecified=simMisspecifiedSet,
		Constraint=simConstraint, Constrain.Parameters.Only=Constrain.Parameters.Only, Misfit.bound=Misfit.bound, Maximum.random=Maximum.random))
}
