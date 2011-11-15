simData <- function(n, param, misspec=new("NullSimMisspec"), equalCon=new("NullSimEqualCon"), conBeforeMis=TRUE, misfitBound=new("NullVector"), maxDraw=100) {
	modelType <- param@modelType
	#browser()
	if(!is.null.object(misspec)) {
		if(modelType != misspec@modelType) stop("SimMisspec and SimSet do not have the same tag")
	}
	if(!is.null.object(equalCon)) {
		if(modelType != equalCon@modelType) stop("SimEqualCon and SimSet do not have the same tag")
	}
	if(!is.null.object(misfitBound)) {
		if(length(misfitBound) == 2) {
			if(misfitBound[1] >= misfitBound[2]) stop("The lower bound is higher than the upper bound")
		} else {
			stop("misfitBound must include only two numbers for lower and upper bound")
		}
	}
	return(new("SimData", n=n, modelType=modelType, param=param, misspec=misspec,
		equalCon=equalCon, conBeforeMis=conBeforeMis, misfitBound=misfitBound, maxDraw=maxDraw))
}
