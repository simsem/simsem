run.misspecified <- function(object, misspec, SimEqualCon=new("NullSimEqualCon"), conBeforeMis=FALSE) {
	if(conBeforeMis) {
		paramSet <- run(object, SimEqualCon, makeList=TRUE)
	} else {
		paramSet <- run(object, makeList=TRUE)
	}
	Output1 <- paramSet[[1]]
	Mis <- run(misspec)
	param <- combine.object(paramSet[[2]], Mis)
	if(!is.null.object(SimEqualCon) & (conBeforeMis=FALSE)) {
		if(object@modelType != SimEqualCon@modelType) stop("Please provide same tags of SimSet and constraint")
		param <- constrain.matrices(param, SimEqualCon)
	}
	Output2 <- fillParam(param, object@modelType)
	return(list(param=Output1, misspec=Output2))
}
