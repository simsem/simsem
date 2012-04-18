fillParam <- function(param, modelType) {
	library(lavaan)
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
			VPS <- diag(PS)
			RPS <- cov2corMod(PS)
		}
		if(isNullObject(TE)) {
			if(isNullObject(VTE)) VTE <- findIndResidualVar(LY, PS, VY) # PS is model-implied covariance
			if(isNullObject(VY)) VY <- findIndTotalVar(LY, PS, VTE)
			TE <- cor2cov(RTE, sqrt(VTE))
		} else {
			VTE <- diag(TE)
			RTE <- cov2corMod(TE)
			VY <- findIndTotalVar(LY, PS, VTE)
		}
		if(isNullObject(MY)) MY <- findIndMean(LY, ME, TY)
		if(isNullObject(TY)) TY <- findIndIntercept(LY, ME, MY)
	} else if(modelType == "Path") {
		if(isNullObject(PS)) {
			if(isNullObject(VPS)) VPS <- findFactorResidualVar(BE, RPS, VE)
			if(isNullObject(VE)) VE <- findFactorTotalVar(BE, RPS, VPS)
			PS <- cor2cov(RPS, suppressWarnings(sqrt(VPS)))
		} else {
			VPS <- diag(PS)
			RPS <- cov2corMod(PS)
			VE <- findFactorTotalVar(BE, RPS, VPS)
		}
		if(isNullObject(ME)) ME <- findFactorMean(BE, AL)
		if(isNullObject(AL)) AL <- findFactorIntercept(BE, ME)
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
			temp.VPS <- findFactorResidualVar(temp.BE, temp.RPS, c(VPH, VE))
			VPS <- temp.VPS[(nx + 1):(nx + ny)]
		}
		if(isNullObject(VE)) {
			temp.VE <- findFactorTotalVar(temp.BE, temp.RPS, c(VPH, VPS))
			VE <- temp.VE[(nx + 1):(nx + ny)]
		}
		if(isNullObject(ME)) {
			temp.ME <- findFactorMean(temp.BE, c(KA, AL))
			ME <- temp.ME[(nx + 1):(nx + ny)]
		}
		if(isNullObject(AL)) {
			temp.AL <- findFactorIntercept(temp.BE, c(KA, ME))
			AL <- temp.AL[(nx + 1):(nx + ny)]
		}
		if(isNullObject(PS)) PS <- cor2cov(RPS, suppressWarnings(sqrt(VPS)))
		if(isNullObject(PH)) PH <- cor2cov(RPH, suppressWarnings(sqrt(VPH)))
	} else if(modelType == "SEM") { 
		if(isNullObject(PS)) {
			if(isNullObject(VPS)) VPS <- findFactorResidualVar(BE, RPS, VE)
			if(isNullObject(VE)) VE <- findFactorTotalVar(BE, RPS, VPS)
			PS <- cor2cov(RPS, suppressWarnings(sqrt(VPS)))
		} else {
			VPS <- diag(PS)
			RPS <- cov2corMod(PS)	
			VE <- findFactorTotalVar(BE, RPS, VPS)			
		}
		if(isNullObject(ME)) ME <- findFactorMean(BE, AL)
		if(isNullObject(AL)) AL <- findFactorIntercept(BE, ME)
		facCov <- findFactorTotalCov(BE, PS)
		if(isNullObject(TE)) {
			if(isNullObject(VTE)) VTE <- findIndResidualVar(LY, facCov, VY)
			if(isNullObject(VY)) VY <- findIndTotalVar(LY, facCov, VTE)
			TE <- cor2cov(RTE, sqrt(VTE))
		} else {
			RTE <- cov2corMod(TE)
			VTE <- diag(TE)
			VY <- findIndTotalVar(LY, facCov, VTE)
		}
		if(isNullObject(MY)) MY <- findIndMean(LY, ME, TY)
		if(isNullObject(TY)) TY <- findIndIntercept(LY, ME, MY)
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
			temp.VPS <- findFactorResidualVar(temp.BE, temp.RPS, c(VPH, VE))
			VPS <- temp.VPS[(nk + 1):(nk + ne)]
		}
		if(isNullObject(VE)) {
			temp.VE <- findFactorTotalVar(temp.BE, temp.RPS, c(VPH, VPS))
			VE <- temp.VE[(nk + 1):(nk + ne)]
		}
		if(isNullObject(ME)) {
			temp.ME <- findFactorMean(temp.BE, c(KA, AL))
			ME <- temp.ME[(nk + 1):(nk + ne)]
		}
		if(isNullObject(AL)) {
			temp.AL <- findFactorIntercept(temp.BE, c(KA, ME))
			AL <- temp.AL[(nk + 1):(nk + ne)]
		}
		if(isNullObject(PS)) PS <- cor2cov(RPS, suppressWarnings(sqrt(VPS)))
		if(isNullObject(PH)) PH <- cor2cov(RPH, suppressWarnings(sqrt(VPH)))
		nk <- nrow(PH)
		ne <- nrow(PS)
		facCov <- findFactorTotalCov(combinePathExoEndo(GA, BE), combineLatentCorExoEndo(PH, PS))
		if(isNullObject(VTE)) VTE <- findIndResidualVar(LY, facCov[(nk+1):(nk+ne), (nk+1):(nk+ne)], VY)
		if(isNullObject(VY)) VY <- findIndTotalVar(LY, facCov[(nk+1):(nk+ne), (nk+1):(nk+ne)], VTE)
		if(isNullObject(MY)) MY <- findIndMean(LY, ME, TY)
		if(isNullObject(TY)) TY <- findIndIntercept(LY, ME, MY)
		if(isNullObject(VTD)) VTD <- findIndResidualVar(LX, facCov[1:nk, 1:nk], VX)
		if(isNullObject(VX)) VX <- findIndTotalVar(LX, facCov[1:nk, 1:nk], VTD)
		if(isNullObject(MX)) MX <- findIndMean(LX, KA, TX)
		if(isNullObject(TX)) TX <- findIndIntercept(LX, KA, MX)
		if(isNullObject(TE)) TE <- cor2cov(RTE, sqrt(VTE))
		if(isNullObject(TD)) TD <- cor2cov(RTD, sqrt(VTD))
		if(isNullObject(TH)) {
			TH <- suppressWarnings(sqrt(diag(VTD)) %*% RTH %*% sqrt(diag(VTE)))
		} else {
			RTH <- suppressWarnings(solve(sqrt(diag(VTD))) %*% TH %*% solve(sqrt(diag(VTE))))
		}
	}
	out <- new("MatrixSet", modelType=modelType, LY=LY, VTE=VTE, TE=TE, RTE=RTE, VY=VY, TY=TY, MY=MY, 
		BE=BE, VPS=VPS, PS=PS, RPS=RPS, VE=VE, AL=AL, ME=ME,
		LX=LX, VTD=VTD, TD=TD, RTD=RTD, VX=VX, TX=TX, MX=MX,
		GA=GA, VPH=VPH, PH=PH, RPH=RPH, KA=KA, TH=TH, RTH=RTH)
	return(out)
}
