expand.matrices <- function(object) {
	if(!is(object, "SimRSet")) stop("The object is not a SimRSet object")
	modelType <- object@modelType
	BE <- object@BE
	AL <- object@AL
	RTE <- new("NullMatrix")
	LY <- object@LY
	TY <- object@TY
	RPH <- new("NullMatrix")
	GA <- object@GA
	KA <- object@KA
	RTD <- new("NullMatrix")
	LX <- object@LX
	TX <- object@TX
	RTH <- object@TH
	RPS <- cov2cor(object@PS)
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
		RTE <- cov2cor(object@TE)
	} 
	if(modelType == "Path.exo" | modelType == "SEM.exo") {
		VPH <- diag(object@PH)
		RPH <- cov2cor(object@PH)
	} 
	if(modelType == "SEM.exo") {
		VTD <- diag(object@TD)
		RTD <- cov2cor(object@TD)
	} 
	if(modelType == "CFA") {
		ME <- AL
		VE <- VPS
		VY <- find.indicator.var(LY, RPS, VTE, VE)
		MY <- find.indicator.mean(LY, ME, TY)
	} else if(modelType == "Path") {
		VE <- find.factor.var(BE, RPS, VPS)
		ME <- find.factor.mean(BE, AL)
	} else if(modelType =="Path.exo") {
		nx <- ncol(GA)
		ny <- nrow(GA)
		temp.BE <- combine.path.exo.endo(GA, BE)
		temp.RPS <- combine.latent.cor.exo.endo(RPH, RPS)
		temp.VE <- find.factor.var(temp.BE, temp.RPS, c(VPH, VPS))
		VE <- temp.VE[(nx + 1):(nx + ny)]
		temp.ME <- find.factor.mean(temp.BE, c(KA, AL))
		ME <- temp.ME[(nx + 1):(nx + ny)]
	} else if(modelType == "SEM") { 
		VE <- find.factor.var(BE, RPS, VPS)
		ME <- find.factor.mean(BE, AL)
		VY <- find.indicator.var(LY, RPS, VTE, VE)
		MY <- find.indicator.mean(LY, ME, TY)
	} else if(modelType == "SEM.exo") {
		nk <- ncol(GA)
		ne <- nrow(GA)
		temp.BE <- combine.path.exo.endo(GA, BE)
		temp.RPS <- combine.latent.cor.exo.endo(RPH, RPS)
		temp.VE <- find.factor.var(temp.BE, temp.RPS, c(VPH, VPS))
		VE <- temp.VE[(nk + 1):(nk + ne)]
		temp.ME <- find.factor.mean(temp.BE, c(KA, AL))
		ME <- temp.ME[(nk + 1):(nk + ne)]
		VY <- find.indicator.var(LY, RPS, VTE, VE)
		MY <- find.indicator.mean(LY, ME, TY)
		VX <- find.indicator.var(LX, RPH, VTD, VPH)
		MX <- find.indicator.mean(LX, KA, TX)
	}
	return(new("MatrixSet", modelType=modelType, LY=LY, VTE=VTE, RTE=RTE, VY=VY, TY=TY, MY=MY, 
	BE=BE, VPS=VPS, RPS=RPS, VE=VE, AL=AL, ME=ME,
	LX=LX, VTD=VTD, RTD=RTD, VX=VX, TX=TX, MX=MX,
	GA=GA, VPH=VPH, RPH=RPH, KA=KA, RTH=RTH))
}
