setClass("simMisspecifiedSet", 
	contains = "simMatrixSet"
)

.NULL.simMisspecifiedSet <- new("simMisspecifiedSet", 
		LY=.NULL.simMatrix,
		TE=.NULL.symMatrix,
		VTE=.NULL.simVector,
		PS=.NULL.symMatrix,
		VPS=.NULL.simVector,
		BE=.NULL.simMatrix,
		TY=.NULL.simVector,
		AL=.NULL.simVector,
		ME=.NULL.simVector,
		MY=.NULL.simVector,
		VE=.NULL.simVector,
		VY=.NULL.simVector,
		LX=.NULL.simMatrix,
		TD=.NULL.symMatrix,
		VTD=.NULL.simVector,
		PH=.NULL.symMatrix,
		GA=.NULL.simMatrix,
		TX=.NULL.simVector,
		KA=.NULL.simVector,
		MX=.NULL.simVector,
		VPH=.NULL.simVector,
		VX=.NULL.simVector,
		TH=.NULL.simMatrix, 
		Tag="NA")

misspecified.CFA.object <- function(...) { #loading, latent.cor, error.cor, latent.var = NULL, error.var = NULL, indicator.var = NULL, intercept = NULL, indicator.mean = NULL, factor.mean = NULL) {
	List <- list(...)
	Names <- names(List)
	keywords <- list(.loading, .error, .latent.cor, c("Variance of Measurement Error", .VTD, .VTE), c("Variance of Indicators", .VX, .VY), .intercept, .factor.mean, c("Means of Indicators", .MX, .MY), c("Variance of Factors", .VE, .VPS, .VPH))
	position <- match.keyword(Names, keywords)
	if(length(position) != length(unique(position))) stop("Some objects were identified more than once.")
	ifelse(contain(1, position), LY <- List[position == 1], LY <- list(.NULL.simMatrix))
	ifelse(contain(2, position), TE <- List[position == 2], TE <- list(.NULL.symMatrix))
	ifelse(contain(3, position), PS <- List[position == 3], PS <- list(.NULL.symMatrix))
	ifelse(contain(4, position), VTE <- List[position == 4], VTE <- list(.NULL.simVector))
	ifelse(contain(5, position), VY <- List[position == 5], VY <- list(.NULL.simVector))
	if(!is.null.object(VTE[[1]]) & !is.null.object(VY[[1]])) stop("Please assign either VTE or VY, not both")
	ifelse(contain(8, position), MY <- List[position == 8], MY <- list(.NULL.simVector))
	ifelse(contain(6, position), TY <- List[position == 6], TY <- list(.NULL.simVector))
	if(!is.null.object(MY[[1]]) & !is.null.object(TY[[1]])) stop("Please assign either MY or TY, not both")
	ifelse(contain(7, position), ME <- List[position == 7], ME <- list(.NULL.simVector))
	ifelse(contain(9, position), VE <- List[position == 9], VE <- list(.NULL.simVector))
	Output <- new("simMisspecifiedSet", LY=LY[[1]], PS=PS[[1]], TE=TE[[1]], VE=VE[[1]], VPS=VE[[1]], VTE=VTE[[1]], VY=VY[[1]], TY=TY[[1]], MY=MY[[1]], ME=ME[[1]], AL=ME[[1]], Tag="CFA")
	return(Output)
}

misspecified.Path.object <- function(..., exo = FALSE) {
	List <- list(...)
	Names <- names(List)
	keywords <- NULL
	if(exo == FALSE) {
		keywords <- list(.BE, .PS, .VPS, .VE, .AL, .ME)
	} else {
		keywords <- list(.BE, .PS, .VPS, .VE, .AL, .ME, .GA, .PH, .VPH, .KA)
	}
	position <- match.keyword(Names, keywords)
	if(length(position) != length(unique(position))) stop("Some objects were identified more than once.")
	ifelse(contain(1, position), BE <- List[position == 1], BE <- list(.NULL.simMatrix))
	ifelse(contain(2, position), PS <- List[position == 2], PS <- list(.NULL.symMatrix))
	ifelse(contain(3, position), VPS <- List[position == 3], VPS <- list(.NULL.simVector))
	ifelse(contain(4, position), VE <- List[position == 4], VE <- list(.NULL.simVector))
	if(!is.null.object(VPS[[1]]) & !is.null.object(VE[[1]])) stop("Please assign either VPS or VE, not both")
	ifelse(contain(6, position), ME <- List[position == 6], ME <- list(.NULL.simVector))
	ifelse(contain(5, position), AL <- List[position == 5], AL <- list(.NULL.simVector))
	if(!is.null.object(ME[[1]]) & !is.null.object(AL[[1]])) stop("Please assign either ME or AL, not both")
	Output <- NULL
	if(exo) {
		ifelse(contain(7, position), GA <- List[position == 7], GA <- list(.NULL.simMatrix))
		ifelse(contain(8, position), PH <- List[position == 8], PH <- list(.NULL.symMatrix))
		ifelse(contain(9, position), VPH <- List[position == 9], VPH <- list(.NULL.simVector))
		ifelse(contain(10, position), KA <- List[position == 10], KA <- list(.NULL.simVector))
		Output <- new("simMisspecifiedSet", BE=BE[[1]], PS=PS[[1]], VPS=VPS[[1]], VE=VE[[1]], AL=AL[[1]], ME=ME[[1]], GA=GA[[1]], PH=PH[[1]], VPH=VPH[[1]], KA=KA[[1]], Tag="Path.exo")	
	} else {
		Output <- new("simMisspecifiedSet", BE=BE[[1]], PS=PS[[1]], VPS=VPS[[1]], VE=VE[[1]], AL=AL[[1]], ME=ME[[1]], Tag="Path")
	}
	return(Output)
}

misspecified.SEM.object <- function(..., exo = FALSE) {
	List <- list(...)
	Names <- names(List)
	keywords <- NULL
	if(exo == FALSE) {
		keywords <- list(.LY, .TE, .VTE, .VY, .TY, .MY, .BE, .PS, .VPS, .VE, .AL, .ME)
	} else {
		keywords <- list(.LY, .TE, .VTE, .VY, .TY, .MY, .BE, .PS, .VPS, .VE, .AL, .ME, .LX, .TD, .VTD, .VX, .TX, .MX, .GA, .PH, .VPH, .KA, .TH)
	}
	position <- match.keyword(Names, keywords)
	if(length(position) != length(unique(position))) stop("Some objects were identified more than once.")
	ifelse(contain(1, position), LY <- List[position == 1], LY <- list(.NULL.simMatrix))
	ifelse(contain(2, position), TE <- List[position == 2], TE <- list(.NULL.symMatrix))
	ifelse(contain(3, position), VTE <- List[position == 3], VTE <- list(.NULL.simVector))
	ifelse(contain(4, position), VY <- List[position == 4], VY <- list(.NULL.simVector))
	if(!is.null.object(VTE[[1]]) & !is.null.object(VY[[1]])) stop("Please assign either VTE or VY, not both")
	ifelse(contain(6, position), MY <- List[position == 6], MY <- list(.NULL.simVector))
	ifelse(contain(5, position), TY <- List[position == 5], TY <- list(.NULL.simVector))
	if(!is.null.object(MY[[1]]) & !is.null.object(TY[[1]])) stop("Please assign either MY or TY, not both")
	ifelse(contain(7, position), BE <- List[position == 7], BE <- list(.NULL.simMatrix))
	ifelse(contain(8, position), PS <- List[position == 8], PS <- list(.NULL.symMatrix))
	ifelse(contain(9, position), VPS <- List[position == 9], VPS <- list(.NULL.simVector))
	ifelse(contain(10, position), VE <- List[position == 10], VE <- list(.NULL.simVector))
	if(!is.null.object(VPS[[1]]) & !is.null.object(VE[[1]])) stop("Please assign either VPS or VE, not both")
	ifelse(contain(12, position), ME <- List[position == 12], ME <- list(.NULL.simVector))
	ifelse(contain(11, position), AL <- List[position == 11], AL <- list(.NULL.simVector))
	if(!is.null.object(ME[[1]]) & !is.null.object(AL[[1]])) stop("Please assign either ME or AL, not both")
	Output <- NULL
	if(exo) {
		ifelse(contain(13, position), LX <- List[position == 13], LX <- list(.NULL.simMatrix))
		ifelse(contain(14, position), TD <- List[position == 14], TD <- list(.NULL.symMatrix))
		ifelse(contain(15, position), VTD <- List[position == 15], VTD <- list(.NULL.simVector))
		ifelse(contain(16, position), VX <- List[position == 16], VX <- list(.NULL.simVector))
		if(!is.null.object(VTD[[1]]) & !is.null.object(VX[[1]])) stop("Please assign either VTD or VX, not both")
		ifelse(contain(18, position), MX <- List[position == 18], MX <- list(.NULL.simVector))
		ifelse(contain(17, position), TX <- List[position == 17], TX <- list(.NULL.simVector))
		if(!is.null.object(MX[[1]]) & !is.null.object(TX[[1]])) stop("Please assign either MX or TX, not both")
		ifelse(contain(19, position), GA <- List[position == 19], GA <- list(.NULL.simMatrix))
		ifelse(contain(20, position), PH <- List[position == 20], PH <- list(.NULL.symMatrix))
		ifelse(contain(21, position), VPH <- List[position == 21], VPH <- list(.NULL.simVector))
		ifelse(contain(22, position), KA <- List[position == 22], KA <- list(.NULL.simVector))
		ifelse(contain(23, position), TH <- List[position == 23], TH <- list(.NULL.simMatrix))
		Output <- new("simMisspecifiedSet", LY=LY[[1]], TE=TE[[1]], VTE=VTE[[1]], VY=VY[[1]], MY=MY[[1]], TY=TY[[1]], BE=BE[[1]], PS=PS[[1]], VPS=VPS[[1]], VE=VE[[1]], AL=AL[[1]], ME=ME[[1]],
					LX=LX[[1]], TD=TD[[1]], VTD=VTD[[1]], VX=VX[[1]], MX=MX[[1]], TX=TX[[1]], GA=GA[[1]], PH=PH[[1]], VPH=VPH[[1]], KA=KA[[1]], TH=TH[[1]], Tag="SEM.exo")	
	} else {
		Output <- new("simMisspecifiedSet", LY=LY[[1]], TE=TE[[1]], VTE=VTE[[1]], VY=VY[[1]], MY=MY[[1]], TY=TY[[1]], BE=BE[[1]], PS=PS[[1]], VPS=VPS[[1]], VE=VE[[1]], AL=AL[[1]], ME=ME[[1]], Tag="SEM")	
	}
	return(Output)
}

setClass("misspecifiedSet", 
	contains = "matrixSet"
)

setMethod("run", signature(object="simMisspecifiedSet"), definition=function(object) {
		LY <- run(object@LY)
		VTE <- run(object@VTE)
		TE <- run(object@TE)
		VY <- run(object@VY)
		TY <- run(object@TY)
		MY <- run(object@MY)
		BE <- run(object@BE)
		VPS <- run(object@VPS)
		PS <- run(object@PS)
		VE <- run(object@VE)
		AL <- run(object@AL)
		ME <- run(object@ME)
		LX <- run(object@LX)
		VTD <- run(object@VTD)
		TD <- run(object@TD)
		VX <- run(object@VX)
		TX <- run(object@TX)
		MX <- run(object@MX)
		GA <- run(object@GA)
		VPH <- run(object@VPH)
		PH <- run(object@PH)
		KA <- run(object@KA)
		TH <- run(object@TH)
		return(new("misspecifiedSet", Tag=object@Tag, LY=LY, VTE=VTE, TE=TE, VY=VY, TY=TY, MY=MY, 
		BE=BE, VPS=VPS, PS=PS, VE=VE, AL=AL, ME=ME,
		LX=LX, VTD=VTD, TD=TD, VX=VX, TX=TX, MX=MX,
		GA=GA, VPH=VPH, PH=PH, KA=KA, TH=TH))
	}
)

setMethod("combine.object", signature(object1="list", object2="misspecifiedSet"), definition=function(object1, object2) {
		LY <- combine.object(object1$LY, object2@LY)
		VTE <- combine.object(object1$VTE, object2@VTE)
		TE <- combine.object(object1$TE, object2@TE, correlation = TRUE)
		VY <- combine.object(object1$VY, object2@VY)
		TY <- combine.object(object1$TY, object2@TY) 
		MY <- combine.object(object1$MY, object2@MY)
		BE <- combine.object(object1$BE, object2@BE)
		VPS <- combine.object(object1$VPS, object2@VPS)
		PS <- combine.object(object1$PS, object2@PS, correlation = TRUE)
		VE <- combine.object(object1$VE, object2@VE) 
		AL <- combine.object(object1$AL, object2@AL) 
		ME <- combine.object(object1$ME, object2@ME) 
		LX <- combine.object(object1$LX, object2@LX) 
		VTD <- combine.object(object1$VTD, object2@VTD) 
		TD <- combine.object(object1$TD, object2@TD, correlation = TRUE)
		VX <- combine.object(object1$VX, object2@VX)
		TX <- combine.object(object1$TX, object2@TX)
		MX <- combine.object(object1$MX, object2@MX)
		GA <- combine.object(object1$GA, object2@GA)
		VPH <- combine.object(object1$VPH, object2@VPH)
		PH <- combine.object(object1$PH, object2@PH, correlation = TRUE)
		KA <- combine.object(object1$KA, object2@KA)
		TH <- combine.object(object1$TH, object2@TH)
		Output <- list(LY=LY, VTE=VTE, TE=TE, VY=VY, TY=TY, MY=MY, 
			BE=BE, VPS=VPS, PS=PS, VE=VE, AL=AL, ME=ME,
			LX=LX, VTD=VTD, TD=TD, VX=VX, TX=TX, MX=MX,
			GA=GA, VPH=VPH, PH=PH, KA=KA, TH=TH)
		return(Output)
	}
)


