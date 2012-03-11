fillParam <- function(param, modelType) {
	LY <- param$LY
	VTE <- param$VTE
	TE <- param$TE
	RTE <- param$RTE
	VY <- param$VY
	TY <- param$TY
	MY <- param$MY
	BE <- param$BE
	VPS <- param$VPS
	PS <- param$PS
	RPS <- param$RPS
	VE <- param$VE
	AL <- param$AL
	ME <- param$ME
	LX <- param$LX
	VTD <- param$VTD
	TD <- param$TD
	RTD <- param$RTD
	VX <- param$VX
	TX <- param$TX
	MX <- param$MX
	GA <- param$GA
	VPH <- param$VPH
	PH <- param$PH
	RPH <- param$RPH
	KA <- param$KA
	TH <- param$TH
	RTH <- param$RTH
	if(modelType == "CFA") {
		if(is.null.object(VTE)) VTE <- find.measurement.error.var(LY, RPS, VY, VE)
		if(is.null.object(VY)) VY <- find.indicator.var(LY, RPS, VTE, VE)
		if(is.null.object(MY)) MY <- find.indicator.mean(LY, ME, TY)
		if(is.null.object(TY)) TY <- find.measurement.intercept(LY, ME, MY)
	} else if(modelType == "Path") {
		if(is.null.object(VPS)) VPS <- find.latent.error.var(BE, RPS, VE)
		if(is.null.object(VE)) VE <- find.factor.var(BE, RPS, VPS)
		if(is.null.object(ME)) ME <- find.factor.mean(BE, AL)
		if(is.null.object(AL)) AL <- find.latent.intercept(BE, ME)
	} else if(modelType =="Path.exo") {
		nx <- ncol(GA)
		ny <- nrow(GA)
		temp.BE <- combine.path.exo.endo(GA, BE)
		temp.RPS <- combine.latent.cor.exo.endo(RPH, RPS)
		if(is.null.object(VPS)) {
			temp.VPS <- find.latent.error.var(temp.BE, temp.RPS, c(VPH, VE))
			VPS <- temp.VPS[(nx + 1):(nx + ny)]
		}
		if(is.null.object(VE)) {
			temp.VE <- find.factor.var(temp.BE, temp.RPS, c(VPH, VPS))
			VE <- temp.VE[(nx + 1):(nx + ny)]
		}
		if(is.null.object(ME)) {
			temp.ME <- find.factor.mean(temp.BE, c(KA, AL))
			ME <- temp.ME[(nx + 1):(nx + ny)]
		}
		if(is.null.object(AL)) {
			temp.AL <- find.latent.intercept(temp.BE, c(KA, ME))
			AL <- temp.AL[(nx + 1):(nx + ny)]
		}
	} else if(modelType == "SEM") { 
		if(is.null.object(VPS)) VPS <- find.latent.error.var(BE, RPS, VE)
		if(is.null.object(VE)) VE <- find.factor.var(BE, RPS, VPS)
		if(is.null.object(ME)) ME <- find.factor.mean(BE, AL)
		if(is.null.object(AL)) AL <- find.latent.intercept(BE, ME)
		if(is.null.object(VTE)) VTE <- find.measurement.error.var(LY, RPS, VY, VE)
		if(is.null.object(VY)) VY <- find.indicator.var(LY, RPS, VTE, VE)
		if(is.null.object(MY)) MY <- find.indicator.mean(LY, ME, TY)
		if(is.null.object(TY)) TY <- find.measurement.intercept(LY, ME, MY)
	} else if(modelType == "SEM.exo") {
		nk <- ncol(GA)
		ne <- nrow(GA)
		temp.BE <- combine.path.exo.endo(GA, BE)
		temp.RPS <- combine.latent.cor.exo.endo(RPH, RPS)
		if(is.null.object(VPS)) {
			temp.VPS <- find.latent.error.var(temp.BE, temp.RPS, c(VPH, VE))
			VPS <- temp.VPS[(nk + 1):(nk + ne)]
		}
		if(is.null.object(VE)) {
			temp.VE <- find.factor.var(temp.BE, temp.RPS, c(VPH, VPS))
			VE <- temp.VE[(nk + 1):(nk + ne)]
		}
		if(is.null.object(ME)) {
			temp.ME <- find.factor.mean(temp.BE, c(KA, AL))
			ME <- temp.ME[(nk + 1):(nk + ne)]
		}
		if(is.null.object(AL)) {
			temp.AL <- find.latent.intercept(temp.BE, c(KA, ME))
			AL <- temp.AL[(nk + 1):(nk + ne)]
		}
		if(is.null.object(VTE)) VTE <- find.measurement.error.var(LY, RPS, VY, VE)
		if(is.null.object(VY)) VY <- find.indicator.var(LY, RPS, VTE, VE)
		if(is.null.object(MY)) MY <- find.indicator.mean(LY, ME, TY)
		if(is.null.object(TY)) TY <- find.measurement.intercept(LY, ME, MY)
		if(is.null.object(VTD)) VTD <- find.measurement.error.var(LX, RPH, VX, VPH)
		if(is.null.object(VX)) VX <- find.indicator.var(LX, RPH, VTD, VPH)
		if(is.null.object(MX)) MX <- find.indicator.mean(LX, KA, TX)
		if(is.null.object(TX)) TX <- find.measurement.intercept(LX, KA, MX)
	}
	out <- new("MatrixSet", modelType=modelType, LY=LY, VTE=VTE, RTE=RTE, VY=VY, TY=TY, MY=MY, 
		BE=BE, VPS=VPS, RPS=RPS, VE=VE, AL=AL, ME=ME,
		LX=LX, VTD=VTD, RTD=RTD, VX=VX, TX=TX, MX=MX,
		GA=GA, VPH=VPH, RPH=RPH, KA=KA, RTH=RTH)
	return(out)
}
