run.misspecified <- function(object, Misspecified, SimEqualCon=new("NullSimEqualCon"), Constrain.Parameters.Only=FALSE, seed=NULL) {
	if(!is.null(seed)) set.seed(seed)
	Parameters <- list(LY = run(object@LY),
		VTE = run(object@VTE),
		TE = run(object@TE),
		VY = run(object@VY),
		TY = run(object@TY),
		MY = run(object@MY),
		BE = run(object@BE),
		VPS = run(object@VPS),
		PS = run(object@PS),
		VE = run(object@VE),
		AL = run(object@AL),
		ME = run(object@ME),
		LX = run(object@LX),
		VTD = run(object@VTD),
		TD = run(object@TD),
		VX = run(object@VX),
		TX = run(object@TX),
		MX = run(object@MX),
		GA = run(object@GA),
		VPH = run(object@VPH),
		PH = run(object@PH),
		KA = run(object@KA),
		TH = run(object@TH))
	if(!is.null.object(SimEqualCon) & (Constrain.Parameters.Only=TRUE)) {
		if(object@modelType != SimEqualCon@modelType) stop("Please provide same tags of SimSet and constraint")
		Parameters <- constrain.matrices(Parameters, SimEqualCon, object@modelType)
	}
	LY <- Parameters$LY
	VTE <- Parameters$VTE
	TE <- Parameters$TE
	VY <- Parameters$VY
	TY <- Parameters$TY
	MY <- Parameters$MY
	BE <- Parameters$BE
	VPS <- Parameters$VPS
	PS <- Parameters$PS
	VE <- Parameters$VE
	AL <- Parameters$AL
	ME <- Parameters$ME
	LX <- Parameters$LX
	VTD <- Parameters$VTD
	TD <- Parameters$TD
	VX <- Parameters$VX
	TX <- Parameters$TX
	MX <- Parameters$MX
	GA <- Parameters$GA
	VPH <- Parameters$VPH
	PH <- Parameters$PH
	KA <- Parameters$KA
	TH <- Parameters$TH
	if(object@modelType == "CFA") {
		if(is.null.object(VTE)) VTE <- find.measurement.error.var(LY, PS, VY, VE)
		if(is.null.object(VY)) VY <- find.indicator.var(LY, PS, VTE, VE)
		if(is.null.object(MY)) MY <- find.indicator.mean(LY, ME, TY)
		if(is.null.object(TY)) TY <- find.measurement.intercept(LY, ME, MY)
	} else if(object@modelType == "Path") {
		if(is.null.object(VPS)) VPS <- find.latent.error.var(BE, PS, VE)
		if(is.null.object(VE)) VE <- find.factor.var(BE, PS, VPS)
		if(is.null.object(ME)) ME <- find.factor.mean(BE, AL)
		if(is.null.object(AL)) AL <- find.latent.intercept(BE, ME)
	} else if(object@modelType =="Path.exo") {
		nx <- ncol(GA)
		ny <- nrow(GA)
		temp.BE <- combine.path.exo.endo(GA, BE)
		temp.PS <- combine.latent.cor.exo.endo(PH, PS)
		if(is.null.object(VPS)) {
			temp.VPS <- find.latent.error.var(temp.BE, temp.PS, c(VPH, VE))
			VPS <- temp.VPS[(nx + 1):(nx + ny)]
		}
		if(is.null.object(VE)) {
			temp.VE <- find.factor.var(temp.BE, temp.PS, c(VPH, VPS))
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
		if(is.null.object(VPS)) VPS <- find.latent.error.var(BE, PS, VE)
		if(is.null.object(VE)) VE <- find.factor.var(BE, PS, VPS)
		if(is.null.object(ME)) ME <- find.factor.mean(BE, AL)
		if(is.null.object(AL)) AL <- find.latent.intercept(BE, ME)
		if(is.null.object(VTE)) VTE <- find.measurement.error.var(LY, PS, VY, VE)
		if(is.null.object(VY)) VY <- find.indicator.var(LY, PS, VTE, VE)
		if(is.null.object(MY)) MY <- find.indicator.mean(LY, ME, TY)
		if(is.null.object(TY)) TY <- find.measurement.intercept(LY, ME, MY)
	} else if(object@modelType == "SEM.exo") {
		nk <- ncol(GA)
		ne <- nrow(GA)
		temp.BE <- combine.path.exo.endo(GA, BE)
		temp.PS <- combine.latent.cor.exo.endo(PH, PS)
		if(is.null.object(VPS)) {
			temp.VPS <- find.latent.error.var(temp.BE, temp.PS, c(VPH, VE))
			VPS <- temp.VPS[(nk + 1):(nk + ne)]
		}
		if(is.null.object(VE)) {
			temp.VE <- find.factor.var(temp.BE, temp.PS, c(VPH, VPS))
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
		if(is.null.object(VTE)) VTE <- find.measurement.error.var(LY, PS, VY, VE)
		if(is.null.object(VY)) VY <- find.indicator.var(LY, PS, VTE, VE)
		if(is.null.object(MY)) MY <- find.indicator.mean(LY, ME, TY)
		if(is.null.object(TY)) TY <- find.measurement.intercept(LY, ME, MY)
		if(is.null.object(VTD)) VTD <- find.measurement.error.var(LX, PH, VX, VPH)
		if(is.null.object(VX)) VX <- find.indicator.var(LX, PH, VTD, VPH)
		if(is.null.object(MX)) MX <- find.indicator.mean(LX, KA, TX)
		if(is.null.object(TX)) TX <- find.measurement.intercept(LX, KA, MX)
	}
	Output1 <- new("MatrixSet", modelType=object@modelType, LY=LY, VTE=VTE, TE=TE, VY=VY, TY=TY, MY=MY, 
		BE=BE, VPS=VPS, PS=PS, VE=VE, AL=AL, ME=ME,
		LX=LX, VTD=VTD, TD=TD, VX=VX, TX=TX, MX=MX,
		GA=GA, VPH=VPH, PH=PH, KA=KA, TH=TH)
	Mis <- run(Misspecified)
	Parameters <- combine.object(Parameters, Mis)
	if(!is.null.object(SimEqualCon) & (Constrain.Parameters.Only=FALSE)) {
		if(object@modelType != SimEqualCon@modelType) stop("Please provide same tags of SimSet and constraint")
		Parameters <- constrain.matrices(Parameters, SimEqualCon, object@modelType)
	}
	LY <- Parameters$LY
	VTE <- Parameters$VTE
	TE <- Parameters$TE
	VY <- Parameters$VY
	TY <- Parameters$TY
	MY <- Parameters$MY
	BE <- Parameters$BE
	VPS <- Parameters$VPS
	PS <- Parameters$PS
	VE <- Parameters$VE
	AL <- Parameters$AL
	ME <- Parameters$ME
	LX <- Parameters$LX
	VTD <- Parameters$VTD
	TD <- Parameters$TD
	VX <- Parameters$VX
	TX <- Parameters$TX
	MX <- Parameters$MX
	GA <- Parameters$GA
	VPH <- Parameters$VPH
	PH <- Parameters$PH
	KA <- Parameters$KA
	TH <- Parameters$TH
	if(object@modelType == "CFA") {
		if(is.null.object(VTE)) VTE <- find.measurement.error.var(LY, PS, VY, VE)
		if(is.null.object(VY)) VY <- find.indicator.var(LY, PS, VTE, VE)
		if(is.null.object(MY)) MY <- find.indicator.mean(LY, ME, TY)
		if(is.null.object(TY)) TY <- find.measurement.intercept(LY, ME, MY)
	} else if(object@modelType == "Path") {
		if(is.null.object(VPS)) VPS <- find.latent.error.var(BE, PS, VE)
		if(is.null.object(VE)) VE <- find.factor.var(BE, PS, VPS)
		if(is.null.object(ME)) ME <- find.factor.mean(BE, AL)
		if(is.null.object(AL)) AL <- find.latent.intercept(BE, ME)
	} else if(object@modelType =="Path.exo") {
		nx <- ncol(GA)
		ny <- nrow(GA)
		temp.BE <- combine.path.exo.endo(GA, BE)
		temp.PS <- combine.latent.cor.exo.endo(PH, PS)
		if(is.null.object(VPS)) {
			temp.VPS <- find.latent.error.var(temp.BE, temp.PS, c(VPH, VE))
			VPS <- temp.VPS[(nx + 1):(nx + ny)]
		}
		if(is.null.object(VE)) {
			temp.VE <- find.factor.var(temp.BE, temp.PS, c(VPH, VPS))
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
		if(is.null.object(VPS)) VPS <- find.latent.error.var(BE, PS, VE)
		if(is.null.object(VE)) VE <- find.factor.var(BE, PS, VPS)
		if(is.null.object(ME)) ME <- find.factor.mean(BE, AL)
		if(is.null.object(AL)) AL <- find.latent.intercept(BE, ME)
		if(is.null.object(VTE)) VTE <- find.measurement.error.var(LY, PS, VY, VE)
		if(is.null.object(VY)) VY <- find.indicator.var(LY, PS, VTE, VE)
		if(is.null.object(MY)) MY <- find.indicator.mean(LY, ME, TY)
		if(is.null.object(TY)) TY <- find.measurement.intercept(LY, ME, MY)
	} else if(object@modelType == "SEM.exo") {
		nk <- ncol(GA)
		ne <- nrow(GA)
		temp.BE <- combine.path.exo.endo(GA, BE)
		temp.PS <- combine.latent.cor.exo.endo(PH, PS)
		if(is.null.object(VPS)) {
			temp.VPS <- find.latent.error.var(temp.BE, temp.PS, c(VPH, VE))
			VPS <- temp.VPS[(nk + 1):(nk + ne)]
		}
		if(is.null.object(VE)) {
			temp.VE <- find.factor.var(temp.BE, temp.PS, c(VPH, VPS))
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
		if(is.null.object(VTE)) VTE <- find.measurement.error.var(LY, PS, VY, VE)
		if(is.null.object(VY)) VY <- find.indicator.var(LY, PS, VTE, VE)
		if(is.null.object(MY)) MY <- find.indicator.mean(LY, ME, TY)
		if(is.null.object(TY)) TY <- find.measurement.intercept(LY, ME, MY)
		if(is.null.object(VTD)) VTD <- find.measurement.error.var(LX, PH, VX, VPH)
		if(is.null.object(VX)) VX <- find.indicator.var(LX, PH, VTD, VPH)
		if(is.null.object(MX)) MX <- find.indicator.mean(LX, KA, TX)
		if(is.null.object(TX)) TX <- find.measurement.intercept(LX, KA, MX)
	}
	Output2 <- new("MatrixSet", modelType=object@modelType, LY=LY, VTE=VTE, TE=TE, VY=VY, TY=TY, MY=MY, 
		BE=BE, VPS=VPS, PS=PS, VE=VE, AL=AL, ME=ME,
		LX=LX, VTD=VTD, TD=TD, VX=VX, TX=TX, MX=MX,
		GA=GA, VPH=VPH, PH=PH, KA=KA, TH=TH)
	return(list(Parameters=Output1, Misspecified=Output2))
}
