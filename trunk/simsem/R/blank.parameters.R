blankParameters <- function(object) {
	if(!is.null.object(object@LY)) object@LY[,] <- NA
	if(!is.null.object(object@TE)) object@TE[,] <- NA
	if(!is.null.object(object@PS)) object@PS[,] <- NA
	if(!is.null.object(object@BE)) object@BE[,] <- NA
	if(!is.null.object(object@TY)) object@TY[] <- NA
	if(!is.null.object(object@AL)) object@AL[] <- NA
	if(!is.null.object(object@LX)) object@LX[,] <- NA
	if(!is.null.object(object@TD)) object@TD[,] <- NA
	if(!is.null.object(object@PH)) object@PH[,] <- NA
	if(!is.null.object(object@GA)) object@GA[,] <- NA
	if(!is.null.object(object@TX)) object@TX[] <- NA
	if(!is.null.object(object@KA)) object@KA[] <- NA
	if(!is.null.object(object@TH)) object@TH[,] <- NA
	return(object)
}
