fillParam <- function(param, modelType) {
	LY <- param@LY
	VTE <- param@VTE
	TE <- param@TE
	RTE <- param@RTE
	VY <- param@VY
	TY <- param@TY
	MY <- param@MY
	BE <- param@BE
	VPS <- param@VPS
	PS <- param@PS
	RPS <- param@RPS
	VE <- param@VE
	AL <- param@AL
	ME <- param@ME
	LX <- param@LX
	VTD <- param@VTD
	TD <- param@TD
	RTD <- param@RTD
	VX <- param@VX
	TX <- param@TX
	MX <- param@MX
	GA <- param@GA
	VPH <- param@VPH
	PH <- param@PH
	RPH <- param@RPH
	KA <- param@KA
	TH <- param@TH
	RTH <- param@RTH
	if(modelType == "CFA") {
		if(isNullObject(PS)) {
			PS <- cor2cov(RPS, sqrt(VE))
		} else {
			VE <- diag(PS)
			RPS <- cov2cor(PS)
		}
		if(isNullObject(TE)) {
			if(isNullObject(VTE)) VTE <- find.measurement.error.var(LY, RPS, VY, VE)
			if(isNullObject(VY)) VY <- find.indicator.var(LY, RPS, VTE, VE)
			TE <- cor2cov(RTE, sqrt(VTE))
		} else {
			VTE <- diag(TE)
			RTE <- cov2cor(TE)
			VY <- find.indicator.var(LY, RPS, VTE, VE)
		}
		if(isNullObject(MY)) MY <- find.indicator.mean(LY, ME, TY)
		if(isNullObject(TY)) TY <- find.measurement.intercept(LY, ME, MY)
	} else if(modelType == "Path") {
		if(isNullObject(PS)) {
			if(isNullObject(VPS)) VPS <- find.latent.error.var(BE, RPS, VE)
			if(isNullObject(VE)) VE <- find.factor.var(BE, RPS, VPS)
			PS <- cor2cov(RPS, sqrt(VPS))
		} else {
			VPS <- diag(PS)
			RPS <- cov2cor(PS)
			VE <- find.factor.var(BE, RPS, VPS)
		}
		if(isNullObject(ME)) ME <- find.factor.mean(BE, AL)
		if(isNullObject(AL)) AL <- find.latent.intercept(BE, ME)
	} else if(modelType =="Path.exo") {
		nx <- ncol(GA)
		ny <- nrow(GA)
		if(!isNullObject(PS)) {
			VPS <- diag(PS)
			RPS <- cov2cor(PS)
		} 
		if(!isNullObject(PH)) {
			VPH <- diag(PH)
			RPH <- cov2cor(PH)
		}
		temp.BE <- combine.path.exo.endo(GA, BE)
		temp.RPS <- combineLatentCorExoEndo(RPH, RPS)
		if(isNullObject(VPS)) {
			temp.VPS <- find.latent.error.var(temp.BE, temp.RPS, c(VPH, VE))
			VPS <- temp.VPS[(nx + 1):(nx + ny)]
		}
		if(isNullObject(VE)) {
			temp.VE <- find.factor.var(temp.BE, temp.RPS, c(VPH, VPS))
			VE <- temp.VE[(nx + 1):(nx + ny)]
		}
		if(isNullObject(ME)) {
			temp.ME <- find.factor.mean(temp.BE, c(KA, AL))
			ME <- temp.ME[(nx + 1):(nx + ny)]
		}
		if(isNullObject(AL)) {
			temp.AL <- find.latent.intercept(temp.BE, c(KA, ME))
			AL <- temp.AL[(nx + 1):(nx + ny)]
		}
		if(isNullObject(PS)) PS <- cor2cov(RPS, sqrt(VPS))
		if(isNullObject(PH)) PH <- cor2cov(RPH, sqrt(VPH))
	} else if(modelType == "SEM") { 
		if(isNullObject(PS)) {
			if(isNullObject(VPS)) VPS <- find.latent.error.var(BE, RPS, VE)
			if(isNullObject(VE)) VE <- find.factor.var(BE, RPS, VPS)
			PS <- cor2cov(RPS, sqrt(VPS))
		} else {
			VPS <- diag(PS)
			RPS <- cov2cor(PS)	
			VE <- find.factor.var(BE, RPS, VPS)			
		}
		if(isNullObject(ME)) ME <- find.factor.mean(BE, AL)
		if(isNullObject(AL)) AL <- find.latent.intercept(BE, ME)
		if(isNullObject(TE)) {
			if(isNullObject(VTE)) VTE <- find.measurement.error.var(LY, RPS, VY, VE)
			if(isNullObject(VY)) VY <- find.indicator.var(LY, RPS, VTE, VE)
			TE <- cor2cov(RTE, sqrt(VTE))
		} else {
			RTE <- cov2cor(TE)
			VTE <- diag(TE)
			VY <- find.indicator.var(LY, RPS, VTE, VE)
		}
		if(isNullObject(MY)) MY <- find.indicator.mean(LY, ME, TY)
		if(isNullObject(TY)) TY <- find.measurement.intercept(LY, ME, MY)
	} else if(modelType == "SEM.exo") {
		nk <- ncol(GA)
		ne <- nrow(GA)
		if(!isNullObject(PS)) {
			VPS <- diag(PS)
			RPS <- cov2cor(PS)
		} 
		if(!isNullObject(PH)) {
			VPH <- diag(PH)
			RPH <- cov2cor(PH)
		}
		if(!isNullObject(TE)) {
			VTE <- diag(TE)
			RTE <- cov2cor(TE)
		} 
		if(!isNullObject(TD)) {
			VTD <- diag(TD)
			RTD <- cov2cor(TD)
		}
		temp.BE <- combine.path.exo.endo(GA, BE)
		temp.RPS <- combineLatentCorExoEndo(RPH, RPS)
		if(isNullObject(VPS)) {
			temp.VPS <- find.latent.error.var(temp.BE, temp.RPS, c(VPH, VE))
			VPS <- temp.VPS[(nk + 1):(nk + ne)]
		}
		if(isNullObject(VE)) {
			temp.VE <- find.factor.var(temp.BE, temp.RPS, c(VPH, VPS))
			VE <- temp.VE[(nk + 1):(nk + ne)]
		}
		if(isNullObject(ME)) {
			temp.ME <- find.factor.mean(temp.BE, c(KA, AL))
			ME <- temp.ME[(nk + 1):(nk + ne)]
		}
		if(isNullObject(AL)) {
			temp.AL <- find.latent.intercept(temp.BE, c(KA, ME))
			AL <- temp.AL[(nk + 1):(nk + ne)]
		}
		if(isNullObject(VTE)) VTE <- find.measurement.error.var(LY, RPS, VY, VE)
		if(isNullObject(VY)) VY <- find.indicator.var(LY, RPS, VTE, VE)
		if(isNullObject(MY)) MY <- find.indicator.mean(LY, ME, TY)
		if(isNullObject(TY)) TY <- find.measurement.intercept(LY, ME, MY)
		if(isNullObject(VTD)) VTD <- find.measurement.error.var(LX, RPH, VX, VPH)
		if(isNullObject(VX)) VX <- find.indicator.var(LX, RPH, VTD, VPH)
		if(isNullObject(MX)) MX <- find.indicator.mean(LX, KA, TX)
		if(isNullObject(TX)) TX <- find.measurement.intercept(LX, KA, MX)
		if(isNullObject(PS)) PS <- cor2cov(RPS, sqrt(VPS))
		if(isNullObject(PH)) PH <- cor2cov(RPH, sqrt(VPH))
		if(isNullObject(TE)) TE <- cor2cov(RTE, sqrt(VTE))
		if(isNullObject(TD)) TD <- cor2cov(RTD, sqrt(VTD))
		if(isNullObject(TH)) {
			TH <- sqrt(diag(VTD)) %*% RTH %*% sqrt(diag(VTE))
		} else {
			RTH <- solve(sqrt(diag(VTD))) %*% TH %*% solve(sqrt(diag(VTE)))		
		}
	}
	out <- new("MatrixSet", modelType=modelType, LY=LY, VTE=VTE, TE=TE, RTE=RTE, VY=VY, TY=TY, MY=MY, 
		BE=BE, VPS=VPS, PS=PS, RPS=RPS, VE=VE, AL=AL, ME=ME,
		LX=LX, VTD=VTD, TD=TD, RTD=RTD, VX=VX, TX=TX, MX=MX,
		GA=GA, VPH=VPH, PH=PH, RPH=RPH, KA=KA, TH=TH, RTH=RTH)
	return(out)
}
