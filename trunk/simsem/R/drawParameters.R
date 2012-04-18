# drawParameters
# function -- simsem package
# Create parameter sets (with or without model misspecification) from simData object
# Argument:
#	object: a simData object
# Return:
#	list of parameters with and without model misspecification
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)

drawParameters <- function(object) {
	modelType <- object@modelType
	param <- NULL
	misspec <- NULL
	implied.CM.param <- NULL
	implied.CM.misspec <- NULL
	misfit <- NULL
	count <- 0
	repeat {
		if(!isNullObject(object@misspec)) {
			Output <- runMisspec(object@param, object@misspec, object@equalCon, object@conBeforeMis)
			param <- Output$param
			misspec <- Output$misspec
			if(validateObject(param) | validateObject(misspec)) {
				param <- reduceMatrices(param)
				misspec <- reduceMatrices(misspec)
				if(!isNullObject(param) && !isNullObject(misspec)) {
					implied.CM.param <- createImpliedMACS(param)
					implied.CM.misspec <- createImpliedMACS(misspec)
					if(sum(eigen(implied.CM.misspec$CM)$values <= 0) == 0) {
						if(isNullObject(object@misfitBound)) {
							break
						} else {
							misfit <- averageMisfit(implied.CM.misspec$M, implied.CM.misspec$CM, 
								implied.CM.param$M, implied.CM.param$CM, countFreeParameters(object@misspec))
							#param <- misspec # Pretend Misspecified as real parameters for data generation
							if(!is.null(misfit) && (misfit > object@misfitBound[1] & misfit < object@misfitBound[2])) break
						}
					}
				}
			}
		} else {
			param <- run(object@param, equalCon=object@equalCon)
			if(validateObject(param)) {
				param <- reduceMatrices(param)
				if(!isNullObject(param)) {
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

