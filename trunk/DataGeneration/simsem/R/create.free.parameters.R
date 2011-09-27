create.free.parameters <-
function(object) {
	if(!is(object, "simMatrixSet")) stop("The attribute is not a simMatrixSet object.")
	LY <- object@LY@Data
	is.measurement.Y <- !(is.null.object(LY))
	TE <- object@TE@Data
	if(is.measurement.Y) {
		VTE <- object@VTE@Data
		ifelse(is.null.object(VTE), diag(TE) <- NA, diag(TE) <- VTE)
	}
	TY <- object@TY@Data
	if(is.measurement.Y & is.default(object@TY)) TY <- rep(NA, nrow(LY))
	BE <- object@BE@Data
	PS <- object@PS@Data 
	VPS <- object@VPS@Data
	ifelse(is.null.object(VPS), ifelse(is.measurement.Y, diag(PS) <- 1, diag(PS) <- NA), diag(PS) <- VPS)
	AL <- object@AL@Data
	if(is.default(object@AL)) {
		ifelse(is.measurement.Y, AL <- rep(0, ncol(PS)), AL <- rep(NA, ncol(PS)))
	}
	#-- Exogeneous Variable --
	LX <- object@LX@Data
	TD <- object@TD@Data 
	GA <- object@GA@Data
	PH <- object@PH@Data	
	KA <- object@KA@Data
	TX <- object@TX@Data
	TH <- object@TH@Data
	if(!is.null.object(PH)) {
		VPH <- object@VPH@Data
		is.measurement.X <- !is.null.object(LX)
		ifelse(is.null.object(VPH) | (sum(VPH != 1) == 0), ifelse(is.measurement.X, diag(PH) <- 1, diag(PH) <- NA), diag(PH) <- VPH)
		if(is.default(object@KA)) ifelse(is.measurement.X, KA <- rep(0, ncol(PH)), KA <- rep(NA, ncol(PH)))
		if(is.measurement.X) {
			VTD <- object@VTD@Data
			ifelse(is.null.object(VTD), diag(TD) <- NA, diag(TD) <- VTD)
			if(is.default(object@TX)) TX <- rep(NA, nrow(LX))
		}
	}
	Output <- new("freeParamSet", LY=LY, TE=TE, BE=BE, PS=PS, AL=AL, TY=TY,
			LX=LX, TD=TD, TX=TX, GA=GA, PH=PH, KA=KA, TH=TH, Tag=object@Tag)	
}

