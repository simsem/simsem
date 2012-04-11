setMethod("simData", signature(param="SimSet"), definition=function(param, n, misspec=new("NullSimMisspec"), equalCon=new("NullSimEqualCon"), conBeforeMis=TRUE, misfitBound=new("NullVector"), maxDraw=100, sequential=NA, facDist=new("NullSimDataDist"), errorDist=new("NullSimDataDist"), indDist=new("NullSimDataDist"), indicatorLab=NULL) {
	modelType <- param@modelType
	if(!(is.na(sequential) | sequential == TRUE | sequential == FALSE)) stop("Please specify NA (to use default), TRUE, or FALSE for the sequential argument")
	if(is.na(sequential)) sequential <- FALSE
	if(!isNullObject(misspec)) {
		if(modelType != misspec@modelType) stop("SimMisspec and SimSet do not have the same tag")
	}
	if(!isNullObject(equalCon)) {
		if(modelType != equalCon@modelType) stop("SimEqualCon and SimSet do not have the same tag")
	}
	if(!isNullObject(misfitBound)) {
		if(length(misfitBound) == 2) {
			if(misfitBound[1] >= misfitBound[2]) stop("The lower bound is higher than the upper bound")
		} else {
			stop("misfitBound must include only two numbers for lower and upper bound")
		}
	}
	if(!isNullObject(errorDist)) {
		if(modelType == "Path" | modelType == "Path.exo") stop("errorDist is not allowed for path analysis model. The distribution of each indicator should be specified in facDist if sequential=TRUE.")
	}
	if(!sequential & !isNullObject(facDist)) stop("facDist is not allowed when using model-implied method in data generation")
	if(!sequential & !isNullObject(errorDist)) stop("errorDist is not allowed when using model-implied method in data generation")
	if(sequential & !isNullObject(indDist)) stop("indDist is not allowed when using sequential method in data generation")
	return(new("SimData", n=n, modelType=modelType, param=param, misspec=misspec,
		equalCon=equalCon, conBeforeMis=conBeforeMis, misfitBound=misfitBound, maxDraw=maxDraw,
		sequential=sequential, facDist=facDist, errorDist=errorDist, indDist=indDist, indicatorLab=indicatorLab))
}
)

setMethod("simData", signature(param="SimModelOut"), definition=function(param, misspec=new("NullSimMisspec"), conBeforeMis=TRUE, misfitBound=new("NullVector"), maxDraw=100, sequential=NA, facDist=new("NullSimDataDist"), errorDist=new("NullSimDataDist"), indDist=new("NullSimDataDist"), usedStd=TRUE) {
	n <- param@n
	modelType <- param@coef@modelType
	equalCon <- param@equalCon
	usedParam <- toSimSet(param, usedStd=usedStd)
	indicatorLab <- param@indicatorLab
	result <- simData(param=usedParam, n=n, misspec=misspec, equalCon=equalCon, conBeforeMis=conBeforeMis, misfitBound=misfitBound, maxDraw=maxDraw, sequential=sequential, facDist=facDist, errorDist=errorDist, indDist=indDist, indicatorLab=indicatorLab) 
	return(result)
}
)
