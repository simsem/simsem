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
			RPS <- cov2corMod(PS)
		}
		if(isNullObject(TE)) {
			if(isNullObject(VTE)) VTE <- findMeasurementErrorVar(LY, RPS, VY, VE)
			if(isNullObject(VY)) VY <- findIndicatorVar(LY, RPS, VTE, VE)
			TE <- cor2cov(RTE, sqrt(VTE))
		} else {
			VTE <- diag(TE)
			RTE <- cov2corMod(TE)
			VY <- findIndicatorVar(LY, RPS, VTE, VE)
		}
		if(isNullObject(MY)) MY <- findIndicatorMean(LY, ME, TY)
		if(isNullObject(TY)) TY <- findMeasurementIntercept(LY, ME, MY)
	} else if(modelType == "Path") {
		if(isNullObject(PS)) {
			if(isNullObject(VPS)) VPS <- findLatentErrorVar(BE, RPS, VE)
			if(isNullObject(VE)) VE <- findFactorVar(BE, RPS, VPS)
			PS <- cor2cov(RPS, sqrt(VPS))
		} else {
			VPS <- diag(PS)
			RPS <- cov2corMod(PS)
			VE <- findFactorVar(BE, RPS, VPS)
		}
		if(isNullObject(ME)) ME <- findFactorMean(BE, AL)
		if(isNullObject(AL)) AL <- findLatentIntercept(BE, ME)
	} else if(modelType =="Path.exo") {
		nx <- ncol(GA)
		ny <- nrow(GA)
		if(!isNullObject(PS)) {
			VPS <- diag(PS)
			RPS <- cov2corMod(PS)
		} 
		if(!isNullObject(PH)) {
			VPH <- diag(PH)
			RPH <- cov2corMod(PH)
		}
		temp.BE <- combinePathExoEndo(GA, BE)
		temp.RPS <- combineLatentCorExoEndo(RPH, RPS)
		if(isNullObject(VPS)) {
			temp.VPS <- findLatentErrorVar(temp.BE, temp.RPS, c(VPH, VE))
			VPS <- temp.VPS[(nx + 1):(nx + ny)]
		}
		if(isNullObject(VE)) {
			temp.VE <- findFactorVar(temp.BE, temp.RPS, c(VPH, VPS))
			VE <- temp.VE[(nx + 1):(nx + ny)]
		}
		if(isNullObject(ME)) {
			temp.ME <- findFactorMean(temp.BE, c(KA, AL))
			ME <- temp.ME[(nx + 1):(nx + ny)]
		}
		if(isNullObject(AL)) {
			temp.AL <- findLatentIntercept(temp.BE, c(KA, ME))
			AL <- temp.AL[(nx + 1):(nx + ny)]
		}
		if(isNullObject(PS)) PS <- cor2cov(RPS, sqrt(VPS))
		if(isNullObject(PH)) PH <- cor2cov(RPH, sqrt(VPH))
	} else if(modelType == "SEM") { 
		if(isNullObject(PS)) {
			if(isNullObject(VPS)) VPS <- findLatentErrorVar(BE, RPS, VE)
			if(isNullObject(VE)) VE <- findFactorVar(BE, RPS, VPS)
			PS <- cor2cov(RPS, sqrt(VPS))
		} else {
			VPS <- diag(PS)
			RPS <- cov2corMod(PS)	
			VE <- findFactorVar(BE, RPS, VPS)			
		}
		if(isNullObject(ME)) ME <- findFactorMean(BE, AL)
		if(isNullObject(AL)) AL <- findLatentIntercept(BE, ME)
		if(isNullObject(TE)) {
			if(isNullObject(VTE)) VTE <- findMeasurementErrorVar(LY, RPS, VY, VE)
			if(isNullObject(VY)) VY <- findIndicatorVar(LY, RPS, VTE, VE)
			TE <- cor2cov(RTE, sqrt(VTE))
		} else {
			RTE <- cov2corMod(TE)
			VTE <- diag(TE)
			VY <- findIndicatorVar(LY, RPS, VTE, VE)
		}
		if(isNullObject(MY)) MY <- findIndicatorMean(LY, ME, TY)
		if(isNullObject(TY)) TY <- findMeasurementIntercept(LY, ME, MY)
	} else if(modelType == "SEM.exo") {
		nk <- ncol(GA)
		ne <- nrow(GA)
		if(!isNullObject(PS)) {
			VPS <- diag(PS)
			RPS <- cov2corMod(PS)
		} 
		if(!isNullObject(PH)) {
			VPH <- diag(PH)
			RPH <- cov2corMod(PH)
		}
		if(!isNullObject(TE)) {
			VTE <- diag(TE)
			RTE <- cov2corMod(TE)
		} 
		if(!isNullObject(TD)) {
			VTD <- diag(TD)
			RTD <- cov2corMod(TD)
		}
		temp.BE <- combinePathExoEndo(GA, BE)
		temp.RPS <- combineLatentCorExoEndo(RPH, RPS)
		if(isNullObject(VPS)) {
			temp.VPS <- findLatentErrorVar(temp.BE, temp.RPS, c(VPH, VE))
			VPS <- temp.VPS[(nk + 1):(nk + ne)]
		}
		if(isNullObject(VE)) {
			temp.VE <- findFactorVar(temp.BE, temp.RPS, c(VPH, VPS))
			VE <- temp.VE[(nk + 1):(nk + ne)]
		}
		if(isNullObject(ME)) {
			temp.ME <- findFactorMean(temp.BE, c(KA, AL))
			ME <- temp.ME[(nk + 1):(nk + ne)]
		}
		if(isNullObject(AL)) {
			temp.AL <- findLatentIntercept(temp.BE, c(KA, ME))
			AL <- temp.AL[(nk + 1):(nk + ne)]
		}
		if(isNullObject(VTE)) VTE <- findMeasurementErrorVar(LY, RPS, VY, VE)
		if(isNullObject(VY)) VY <- findIndicatorVar(LY, RPS, VTE, VE)
		if(isNullObject(MY)) MY <- findIndicatorMean(LY, ME, TY)
		if(isNullObject(TY)) TY <- findMeasurementIntercept(LY, ME, MY)
		if(isNullObject(VTD)) VTD <- findMeasurementErrorVar(LX, RPH, VX, VPH)
		if(isNullObject(VX)) VX <- findIndicatorVar(LX, RPH, VTD, VPH)
		if(isNullObject(MX)) MX <- findIndicatorMean(LX, KA, TX)
		if(isNullObject(TX)) TX <- findMeasurementIntercept(LX, KA, MX)
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
