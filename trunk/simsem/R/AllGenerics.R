setGeneric("run", function(object, ...) { 
	return(standardGeneric("run")) 
} )

setGeneric("adjust", function(target, param, pos, numAsFixed=TRUE) { 
	return(standardGeneric("adjust")) 
} )

setGeneric("combine.object", function(object1, object2, ...) { 
	return(standardGeneric("combine.object")) 
} )

setGeneric("constrain.matrices", function(object, SimEqualCon, ...) { 
	return(standardGeneric("constrain.matrices")) 
} )

setGeneric("count.random.object", function(object, ...) { 
	return(standardGeneric("count.random.object")) 
} )

setGeneric("createImpliedMACS", function(object) { 
	return(standardGeneric("createImpliedMACS")) 
} )

setGeneric("find.OpenMx.values", function(param, start) { 
	return(standardGeneric("find.OpenMx.values")) 
} )

setGeneric("is.null.object", function(target) { 
	return(standardGeneric("is.null.object")) 
} )

setGeneric("divide.object", function(object, constant, ...) { 
	return(standardGeneric("divide.object")) 
} )

setGeneric("make.labels", function(object, ...) { 
	return(standardGeneric("make.labels")) 
} )

setGeneric("simModel", function(object, ...) { 
	return(standardGeneric("simModel")) 
} )

setGeneric("starting.values", function(object, trial, ...) { 
	return(standardGeneric("starting.values")) 
} )

setGeneric("summaryShort", function(object, ...) { 
	return(standardGeneric("summaryShort")) 
} )

setGeneric("tag.headers", function(object, ...) { 
	return(standardGeneric("tag.headers")) 
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

setGeneric("vectorize.object", function(object, labels, ...) { 
	return(standardGeneric("vectorize.object")) 
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
