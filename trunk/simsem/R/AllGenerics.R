setGeneric("run", function(object, ...) { 
	return(standardGeneric("run")) 
} )

setGeneric("adjust", function(target, param, pos, numAsFixed=TRUE) { 
	return(standardGeneric("adjust")) 
} )

setGeneric("combineObject", function(object1, object2, ...) { 
	return(standardGeneric("combineObject")) 
} )

setGeneric("constrainMatrices", function(object, SimEqualCon, ...) { 
	return(standardGeneric("constrainMatrices")) 
} )

setGeneric("countRandomObject", function(object, ...) { 
	return(standardGeneric("countRandomObject")) 
} )

setGeneric("createImpliedMACS", function(object, ...) { 
	return(standardGeneric("createImpliedMACS")) 
} )

setGeneric("findOpenMxValues", function(param, start) { 
	return(standardGeneric("findOpenMxValues")) 
} )

setGeneric("isNullObject", function(target) { 
	return(standardGeneric("isNullObject")) 
} )

setGeneric("divideObject", function(object, constant, ...) { 
	return(standardGeneric("divideObject")) 
} )

setGeneric("makeLabels", function(object, ...) { 
	return(standardGeneric("makeLabels")) 
} )

setGeneric("simModel", function(object, ...) { 
	return(standardGeneric("simModel")) 
} )

setGeneric("startingValues", function(object, trial, ...) { 
	return(standardGeneric("startingValues")) 
} )

setGeneric("summaryShort", function(object, ...) { 
	return(standardGeneric("summaryShort")) 
} )

setGeneric("tagHeaders", function(object, ...) { 
	return(standardGeneric("tagHeaders")) 
} )

setGeneric("getCutoff", function(object, alpha, revDirec=FALSE, usedFit=NULL) { 
	return(standardGeneric("getCutoff")) 
} )

setGeneric("plotCutoff", function(object, ...) { 
	return(standardGeneric("plotCutoff")) 
} )

setGeneric("plotPower", function(altObject, nullObject, ...) { 
	return(standardGeneric("plotPower")) 
} )

setGeneric("getPower", function(altObject, cutoff, revDirec = FALSE, usedFit=NULL) { 
	return(standardGeneric("getPower")) 
} )

setGeneric("vectorizeObject", function(object, labels, ...) { 
	return(standardGeneric("vectorizeObject")) 
} )

setGeneric("summaryParam", function(object, ...) { 
	return(standardGeneric("summaryParam")) 
} )

setGeneric("subtractObject", function(object1, object2, ...) { 
	return(standardGeneric("subtractObject")) 
} )

setGeneric("standardize", function(object) { 
	return(standardGeneric("standardize")) 
} )

setGeneric("extract", function(object, ...) { 
	return(standardGeneric("extract")) 
} )

setGeneric("plotDist", function(object, ...) { 
	return(standardGeneric("plotDist")) 
} )

setGeneric("skew", function(object, ...) { 
	return(standardGeneric("skew")) 
} )

setGeneric("kurtosis", function(object, ...) { 
	return(standardGeneric("kurtosis")) 
} )

