# drawParameters
# function -- simsem package
# Create parameter sets (with or without model misspecification) from simData object
# Argument:
#	object: a simData object
# Return:
#	list of parameters with and without model misspecification
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: February 21, 2012

drawParameters <- function(object) {
	modelType <- object@modelType
	param <- NULL
	misspec <- NULL
	implied.CM.param <- NULL
	implied.CM.misspec <- NULL
	misfit <- NULL
	count <- 0
	repeat {
		#browser()
		if(!is.null.object(object@misspec)) {
			Output <- run.misspecified(object@param, object@misspec, object@equalCon, object@conBeforeMis)
			param <- Output$param
			misspec <- Output$misspec
			if(validate.object(param) | validate.object(misspec)) {
				param <- reduce.matrices(param)
				misspec <- reduce.matrices(misspec)
				if(!is.null.object(param) && !is.null.object(misspec)) {
					implied.CM.param <- createImpliedMACS(param)
					implied.CM.misspec <- createImpliedMACS(misspec)
					if(sum(eigen(implied.CM.misspec$CM)$values <= 0) == 0) {
						if(is.null.object(object@misfitBound)) {
							break
						} else {
							misfit <- average.misfit(implied.CM.misspec$M, implied.CM.misspec$CM, 
								implied.CM.param$M, implied.CM.param$CM, count.random.object(object@misspec))
							#param <- misspec # Pretend Misspecified as real parameters for data generation
							if(!is.null(misfit) && (misfit > object@misfitBound[1] & misfit < object@misfitBound[2])) break
						}
					}
				}
			}
		} else {
			param <- run(object@param, equalCon=object@equalCon)
			if(validate.object(param)) {
				param <- reduce.matrices(param)
				if(!is.null.object(param)) {
					implied.CM.param <- createImpliedMACS(param)
					implied.CM.misspec <- implied.CM.param
					if(sum(eigen(implied.CM.param$CM)$values <= 0) == 0) break
				}
			}
		}
		count <- count + 1
		if(count > object@maxDraw) stop("The model cannot make a good set of parameters within limit of maximum random sampling of parameters")
	}
	return(list(real=param, misspec=misspec))
}

