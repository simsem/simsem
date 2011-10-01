setMethod("model.object", signature(object="freeParamSet"), definition=function(object, Starting.Values = NULL, Constraint=new("nullSimConstraint"), Program="lavaan") {
	Tag <- object@Tag
	if(!is.null(Starting.Values)) {
		if(Tag != Starting.Values@Tag) stop("Starting Values and Parameters do not have the same tag")
	} else {
		Starting.Values <- default.starting.values(object)
	}
	if(!is.null.object(simConstraint)) {
		if(Tag != Constraint@Tag) stop("simConstraint and freeParamSet do not have the same tag")
	}
	return(new("simModel", Tag=Tag, Parameters=object, Starting.Values=Starting.Values, Constraint=Constraint, Program=Program))
})

setMethod("model.object", signature(object="simMatrixSet"), definition=function(object, Constraint=new("nullSimConstraint"), Program="lavaan", trial=10) {
	#browser()
	Starting.Values <- starting.values(object, trial)
	Starting.Values <- reduce.matrices(Starting.Values)
	#browser()
	freeParameters <- create.free.parameters(object)
	Tag <- object@Tag
	if(!is.null.object(Constraint)) {
		if(Tag != Constraint@Tag) stop("simConstraint and simMatrixSet do not have the same tag")
	}
	return(new("simModel", Tag=Tag, Parameters=freeParameters, Starting.Values=Starting.Values, Constraint=Constraint, Program=Program))
})
