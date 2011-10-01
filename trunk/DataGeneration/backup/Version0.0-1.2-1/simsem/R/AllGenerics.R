setGeneric("run", function(object, ...) { 
	return(standardGeneric("run")) 
} )

setGeneric("adjust.object", function(target, simDist, position, constant.fixed) { 
	return(standardGeneric("adjust.object")) 
} )

setGeneric("combine.object", function(object1, object2, ...) { 
	return(standardGeneric("combine.object")) 
} )

setGeneric("constrain.matrices", function(object, simConstraint, ...) { 
	return(standardGeneric("constrain.matrices")) 
} )

setGeneric("count.random.object", function(object) { 
	return(standardGeneric("count.random.object")) 
} )

setGeneric("create.implied.MACS", function(object) { 
	return(standardGeneric("create.implied.MACS")) 
} )

setGeneric("find.OpenMx.values", function(Parameters, Starting.Values) { 
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

setGeneric("model.object", function(object, ...) { 
	return(standardGeneric("model.object")) 
} )

setGeneric("starting.values", function(object, trial, ...) { 
	return(standardGeneric("starting.values")) 
} )

setGeneric("summary.short", function(object) { 
	return(standardGeneric("summary.short")) 
} )

setGeneric("tag.headers", function(object, ...) { 
	return(standardGeneric("tag.headers")) 
} )
