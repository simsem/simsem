run.misspecified <- function(object, misspec, SimEqualCon=new("NullSimEqualCon"), conBeforeMis=FALSE, seed=NULL) {
	if(!is.null(seed)) set.seed(seed)
	param <- list(LY = run(object@LY),
		VTE = run(object@VTE),
		RTE = run(object@RTE),
		VY = run(object@VY),
		TY = run(object@TY),
		MY = run(object@MY),
		BE = run(object@BE),
		VPS = run(object@VPS),
		RPS = run(object@RPS),
		VE = run(object@VE),
		AL = run(object@AL),
		ME = run(object@ME),
		LX = run(object@LX),
		VTD = run(object@VTD),
		RTD = run(object@RTD),
		VX = run(object@VX),
		TX = run(object@TX),
		MX = run(object@MX),
		GA = run(object@GA),
		VPH = run(object@VPH),
		RPH = run(object@RPH),
		KA = run(object@KA),
		RTH = run(object@RTH))
	if(!is.null.object(SimEqualCon) & (conBeforeMis=TRUE)) {
		if(object@modelType != SimEqualCon@modelType) stop("Please provide same tags of SimSet and constraint")
		param <- constrain.matrices(param, SimEqualCon, object@modelType)
	}
	LY <- param$LY
	VTE <- param$VTE
	RTE <- param$RTE
	VY <- param$VY
	TY <- param$TY
	MY <- param$MY
	BE <- param$BE
	VPS <- param$VPS
	RPS <- param$RPS
	VE <- param$VE
	AL <- param$AL
	ME <- param$ME
	LX <- param$LX
	VTD <- param$VTD
	RTD <- param$RTD
	VX <- param$VX
	TX <- param$TX
	MX <- param$MX
	GA <- param$GA
	VPH <- param$VPH
	RPH <- param$RPH
	KA <- param$KA
	RTH <- param$RTH
	if(object@modelType == "CFA") {
		if(is.null.object(VTE)) VTE <- find.measurement.error.var(LY, RPS, VY, VE)
		if(is.null.object(VY)) VY <- find.indicator.var(LY, RPS, VTE, VE)
		if(is.null.object(MY)) MY <- find.indicator.mean(LY, ME, TY)
		if(is.null.object(TY)) TY <- find.measurement.intercept(LY, ME, MY)
	} else if(object@modelType == "Path") {
		if(is.null.object(VPS)) VPS <- find.latent.error.var(BE, RPS, VE)
		if(is.null.object(VE)) VE <- find.factor.var(BE, RPS, VPS)
		if(is.null.object(ME)) ME <- find.factor.mean(BE, AL)
		if(is.null.object(AL)) AL <- find.latent.intercept(BE, ME)
	} else if(object@modelType =="Path.exo") {
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
	} else if(object@modelType == "SEM") { 
		if(is.null.object(VPS)) VPS <- find.latent.error.var(BE, RPS, VE)
		if(is.null.object(VE)) VE <- find.factor.var(BE, RPS, VPS)
		if(is.null.object(ME)) ME <- find.factor.mean(BE, AL)
		if(is.null.object(AL)) AL <- find.latent.intercept(BE, ME)
		if(is.null.object(VTE)) VTE <- find.measurement.error.var(LY, RPS, VY, VE)
		if(is.null.object(VY)) VY <- find.indicator.var(LY, RPS, VTE, VE)
		if(is.null.object(MY)) MY <- find.indicator.mean(LY, ME, TY)
		if(is.null.object(TY)) TY <- find.measurement.intercept(LY, ME, MY)
	} else if(object@modelType == "SEM.exo") {
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
	Output1 <- new("MatrixSet", modelType=object@modelType, LY=LY, VTE=VTE, RTE=RTE, VY=VY, TY=TY, MY=MY, 
		BE=BE, VPS=VPS, RPS=RPS, VE=VE, AL=AL, ME=ME,
		LX=LX, VTD=VTD, RTD=RTD, VX=VX, TX=TX, MX=MX,
		GA=GA, VPH=VPH, RPH=RPH, KA=KA, RTH=RTH)
	Mis <- run(misspec)
	param <- combine.object(param, Mis)
	if(!is.null.object(SimEqualCon) & (conBeforeMis=FALSE)) {
		if(object@modelType != SimEqualCon@modelType) stop("Please provide same tags of SimSet and constraint")
		param <- constrain.matrices(param, SimEqualCon, object@modelType)
	}
	LY <- param$LY
	VTE <- param$VTE
	RTE <- param$RTE
	VY <- param$VY
	TY <- param$TY
	MY <- param$MY
	BE <- param$BE
	VPS <- param$VPS
	RPS <- param$RPS
	VE <- param$VE
	AL <- param$AL
	ME <- param$ME
	LX <- param$LX
	VTD <- param$VTD
	RTD <- param$RTD
	VX <- param$VX
	TX <- param$TX
	MX <- param$MX
	GA <- param$GA
	VPH <- param$VPH
	RPH <- param$RPH
	KA <- param$KA
	RTH <- param$RTH
	if(object@modelType == "CFA") {
		if(is.null.object(VTE)) VTE <- find.measurement.error.var(LY, RPS, VY, VE)
		if(is.null.object(VY)) VY <- find.indicator.var(LY, RPS, VTE, VE)
		if(is.null.object(MY)) MY <- find.indicator.mean(LY, ME, TY)
		if(is.null.object(TY)) TY <- find.measurement.intercept(LY, ME, MY)
	} else if(object@modelType == "Path") {
		if(is.null.object(VPS)) VPS <- find.latent.error.var(BE, RPS, VE)
		if(is.null.object(VE)) VE <- find.factor.var(BE, RPS, VPS)
		if(is.null.object(ME)) ME <- find.factor.mean(BE, AL)
		if(is.null.object(AL)) AL <- find.latent.intercept(BE, ME)
	} else if(object@modelType =="Path.exo") {
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
	} else if(object@modelType == "SEM") { 
		if(is.null.object(VPS)) VPS <- find.latent.error.var(BE, RPS, VE)
		if(is.null.object(VE)) VE <- find.factor.var(BE, RPS, VPS)
		if(is.null.object(ME)) ME <- find.factor.mean(BE, AL)
		if(is.null.object(AL)) AL <- find.latent.intercept(BE, ME)
		if(is.null.object(VTE)) VTE <- find.measurement.error.var(LY, RPS, VY, VE)
		if(is.null.object(VY)) VY <- find.indicator.var(LY, RPS, VTE, VE)
		if(is.null.object(MY)) MY <- find.indicator.mean(LY, ME, TY)
		if(is.null.object(TY)) TY <- find.measurement.intercept(LY, ME, MY)
	} else if(object@modelType == "SEM.exo") {
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
	Output2 <- new("MatrixSet", modelType=object@modelType, LY=LY, VTE=VTE, RTE=RTE, VY=VY, TY=TY, MY=MY, 
		BE=BE, VPS=VPS, RPS=RPS, VE=VE, AL=AL, ME=ME,
		LX=LX, VTD=VTD, RTD=RTD, VX=VX, TX=TX, MX=MX,
		GA=GA, VPH=VPH, RPH=RPH, KA=KA, RTH=RTH)
	return(list(param=Output1, misspec=Output2))
}
