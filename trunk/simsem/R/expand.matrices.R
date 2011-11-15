expand.matrices <- function(object) {
	if(!is(object, "SimRSet")) stop("The object is not a SimRSet object")
	modelType <- object@modelType
	BE <- object@BE
	AL <- object@AL
	TE <- new("NullMatrix")
	LY <- object@LY
	TY <- object@TY
	PH <- new("NullMatrix")
	GA <- object@GA
	KA <- object@KA
	TD <- new("NullMatrix")
	LX <- object@LX
	TX <- object@TX
	TH <- object@TH
	PS <- cov2cor(object@PS)
	VPS <- diag(object@PS)
	VPH <- new("NullVector")
	VTD <- new("NullVector")
	VTE <- new("NullVector")
	VX <- new("NullVector")
	VY <- new("NullVector")
	VE <- new("NullVector")
	MX <- new("NullVector")
	MY <- new("NullVector")
	ME <- new("NullVector")
	if(modelType == "CFA" | modelType == "SEM" | modelType == "SEM.exo") {
		VTE <- diag(object@TE)
		TE <- cov2cor(object@TE)
	} 
	if(modelType == "Path.exo" | modelType == "SEM.exo") {
		VPH <- diag(object@PH)
		PH <- cov2cor(object@PH)
	} 
	if(modelType == "SEM.exo") {
		VTD <- diag(object@TD)
		TD <- cov2cor(object@TD)
	} 
	if(modelType == "CFA") {
		ME <- AL
		VE <- VPS
		VY <- find.indicator.var(LY, PS, VTE, VE)
		MY <- find.indicator.mean(LY, ME, TY)
	} else if(modelType == "Path") {
		VE <- find.factor.var(BE, PS, VPS)
		ME <- find.factor.mean(BE, AL)
	} else if(modelType =="Path.exo") {
		nx <- ncol(GA)
		ny <- nrow(GA)
		temp.BE <- combine.path.exo.endo(GA, BE)
		temp.PS <- combine.latent.cor.exo.endo(PH, PS)
		temp.VE <- find.factor.var(temp.BE, temp.PS, c(VPH, VPS))
		VE <- temp.VE[(nx + 1):(nx + ny)]
		temp.ME <- find.factor.mean(temp.BE, c(KA, AL))
		ME <- temp.ME[(nx + 1):(nx + ny)]
	} else if(modelType == "SEM") { 
		VE <- find.factor.var(BE, PS, VPS)
		ME <- find.factor.mean(BE, AL)
		VY <- find.indicator.var(LY, PS, VTE, VE)
		MY <- find.indicator.mean(LY, ME, TY)
	} else if(modelType == "SEM.exo") {
		nk <- ncol(GA)
		ne <- nrow(GA)
		temp.BE <- combine.path.exo.endo(GA, BE)
		temp.PS <- combine.latent.cor.exo.endo(PH, PS)
		temp.VE <- find.factor.var(temp.BE, temp.PS, c(VPH, VPS))
		VE <- temp.VE[(nk + 1):(nk + ne)]
		temp.ME <- find.factor.mean(temp.BE, c(KA, AL))
		ME <- temp.ME[(nk + 1):(nk + ne)]
		VY <- find.indicator.var(LY, PS, VTE, VE)
		MY <- find.indicator.mean(LY, ME, TY)
		VX <- find.indicator.var(LX, PH, VTD, VPH)
		MX <- find.indicator.mean(LX, KA, TX)
	}
	return(new("MatrixSet", modelType=modelType, LY=LY, VTE=VTE, TE=TE, VY=VY, TY=TY, MY=MY, 
	BE=BE, VPS=VPS, PS=PS, VE=VE, AL=AL, ME=ME,
	LX=LX, VTD=VTD, TD=TD, VX=VX, TX=TX, MX=MX,
	GA=GA, VPH=VPH, PH=PH, KA=KA, TH=TH))
}
