expand.matrices <- function(object) {
	if(!is(object, "reducedMatrixSet")) stop("The object is not a reducedMatrixSet object")
	Tag <- object@Tag
	BE <- object@BE
	AL <- object@AL
	TE <- new("nullMatrix")
	LY <- object@LY
	TY <- object@TY
	PH <- new("nullMatrix")
	GA <- object@GA
	KA <- object@KA
	TD <- new("nullMatrix")
	LX <- object@LX
	TX <- object@TX
	TH <- object@TH
	PS <- cov2cor(object@PS)
	VPS <- diag(object@PS)
	VPH <- new("nullVector")
	VTD <- new("nullVector")
	VTE <- new("nullVector")
	VX <- new("nullVector")
	VY <- new("nullVector")
	VE <- new("nullVector")
	MX <- new("nullVector")
	MY <- new("nullVector")
	ME <- new("nullVector")
	if(Tag == "CFA" | Tag == "SEM" | Tag == "SEM.exo") {
		VTE <- diag(object@TE)
		TE <- cov2cor(object@TE)
	} 
	if(Tag == "Path.exo" | Tag == "SEM.exo") {
		VPH <- diag(object@PH)
		PH <- cov2cor(object@PH)
	} 
	if(Tag == "SEM.exo") {
		VTD <- diag(object@TD)
		TD <- cov2cor(object@TD)
	} 
	if(Tag == "CFA") {
		ME <- AL
		VE <- VPS
		VY <- find.indicator.var(LY, PS, VTE, VE)
		MY <- find.indicator.mean(LY, ME, TY)
	} else if(Tag == "Path") {
		VE <- find.factor.var(BE, PS, VPS)
		ME <- find.factor.mean(BE, AL)
	} else if(Tag =="Path.exo") {
		nx <- ncol(GA)
		ny <- nrow(GA)
		temp.BE <- combine.path.exo.endo(GA, BE)
		temp.PS <- combine.latent.cor.exo.endo(PH, PS)
		temp.VE <- find.factor.var(temp.BE, temp.PS, c(VPH, VPS))
		VE <- temp.VE[(nx + 1):(nx + ny)]
		temp.ME <- find.factor.mean(temp.BE, c(KA, AL))
		ME <- temp.ME[(nx + 1):(nx + ny)]
	} else if(Tag == "SEM") { 
		VE <- find.factor.var(BE, PS, VPS)
		ME <- find.factor.mean(BE, AL)
		VY <- find.indicator.var(LY, PS, VTE, VE)
		MY <- find.indicator.mean(LY, ME, TY)
	} else if(Tag == "SEM.exo") {
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
	return(new("matrixSet", Tag=Tag, LY=LY, VTE=VTE, TE=TE, VY=VY, TY=TY, MY=MY, 
	BE=BE, VPS=VPS, PS=PS, VE=VE, AL=AL, ME=ME,
	LX=LX, VTD=VTD, TD=TD, VX=VX, TX=TX, MX=MX,
	GA=GA, VPH=VPH, PH=PH, KA=KA, TH=TH))
}
