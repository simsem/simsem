# simSetCFA
# Function -- simsem package
# Create a set of matrice that belongs to CFA model.
# Function: simSetCFA(...)
# Argument:
#	...:	All matrices that belongs to CFA model, see details below.
# Return: 	SimSet.c containing CFA model (with "CFA" tag)
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: March 10, 2012

simSetCFA <- function(...) { 
	W <- getKeywords()
	List <- list(...)
	Names <- names(List)
	keywords <- list(W$loading, W$errorCor, W$facCor, W$errorVar, W$indicatorVar, W$intercept, W$facMean, W$indicatorMean, W$facVar, W$errorCov, W$facCov) # 11 total
	position <- matchKeywords(Names, keywords)
	if(length(position) != length(unique(position))) stop("Some objects were identified more than once.")
	ifelse(contain(1, position), LY <- List[position == 1], stop("No loading object in CFA"))
	ni <- nrow(run(LY[[1]]))
	nk <- ncol(run(LY[[1]]))
	if(contain(10, position)) {
		TE <- List[position == 10]
		ifelse(contain(2, position), stop("Error covariance and error correlation cannot be specified at the same time!"), RTE <- list(new("NullSymMatrix")))
		ifelse(contain(4, position), stop("Error covariance and error variance cannot be specified at the same time!"), VTE <- list(new("NullSimVector")))		
		ifelse(contain(5, position), stop("Error covariance and total indicator variance cannot be specified at the same time!"), VY <- list(new("NullSimVector")))
	} else {
		TE <- list(new("NullSymMatrix"))
		ifelse(contain(2, position), RTE <- List[position == 2], stop("No error correlation object in CFA"))
		ifelse(contain(4, position), VTE <- List[position == 4], VTE <- list(new("NullSimVector")))	
		ifelse(contain(5, position), VY <- List[position == 5], ifelse(isNullObject(VTE[[1]]), { VY <- list(freeVector(1, ni)); comment(VY[[1]]) <- "default"}, VY <- list(new("NullSimVector"))))
	}
	if(contain(11, position)) {
		PS <- List[position == 11]
		ifelse(contain(3, position), stop("Factor covariance and factor correlation cannot be specified at the same time!"), RPS <- list(new("NullSymMatrix")))
		ifelse(contain(9, position), stop("Factor covariance and factor variance cannot be specified at the same time!"), VE <- list(new("NullSimVector")))		
	} else {
		PS <- list(new("NullSymMatrix"))
		ifelse(contain(3, position), RPS <- List[position == 3], stop("No latent variables correlation object in CFA"))
		ifelse(contain(9, position), VE <- List[position == 9], { VE <- list(constantVector(1, nk)); comment(VE[[1]]) <- "default"})
	}
	ifelse(contain(8, position), MY <- List[position == 8], MY <- list(new("NullSimVector")))
	ifelse(contain(6, position), TY <- List[position == 6], ifelse(isNullObject(MY[[1]]), { TY <- list(freeVector(0, ni)); comment(TY[[1]]) <- "default"}, TY <- list(new("NullSimVector"))))
	ifelse(contain(7, position), ME <- List[position == 7], { ME <- list(constantVector(0, nk)); comment(ME[[1]]) <- "default"})
	Output <- new("SimSet", LY=LY[[1]], PS=PS[[1]], RPS=RPS[[1]], TE=TE[[1]], RTE=RTE[[1]], VE=VE[[1]], VPS=VE[[1]], VTE=VTE[[1]], VY=VY[[1]], TY=TY[[1]], MY=MY[[1]], ME=ME[[1]], AL=ME[[1]], modelType="CFA")
	return(Output)
}

#Details of the names of elements in CFA models:
#	###CFA object can be either specified in X or Y side.
#	REQUIRED: LX or LY for factor loading matrix (need to be SimMatrix.c object). 
#	TD or TE
#	REQUIRED: RTD or RTE for measurement error correlation matrix (need to be SymMatrix.c object).
#	PH or PS
#	REQUIRED: RPH or RPS for factor correlation matrix (need to be SymMatrix.c object).
#	VTD or VTE for measurement error variance (need to be SimVector.c object).
#	VX or VY for total indicator variance (need to be SimVector.c object).
#	NOTE: Either measurement error variance or indicator variance is specified. Both cannot be simultaneously specified.
#	VPH, VPS, VK, or VE for factor total variance (need to be SimVector.c object).
#	TX or TY for measurement intercepts. (need to be SimVector.c object).
#	MX or MY for overall indicator means. (need to be SimVector.c object).
#	NOTE: Either measurement intercept of indicator mean can be specified. Both cannot be specified simultaneously.
#	KA, AL, MK, or ME for factor means (need to be SimVector object).

#DEFAULT:
# 	1) All indicator variances are equal to 1. Measurement error variances are automatically implied from total indicator variances.
#	2) All measurement error variances are free parameters.
#	3) All indicator means are equal to 0. Indicator intercepts are automatically implied from indicator means.
#	4) All indicator intercepts are free parameters.
#	5) All factor variances are equal to 1.
#	6) All factor variances are fixed.
#	7) All factor means are equal to 0.
#	8) All factor means are fixed.

#Examples:
#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#loadingValues[1:3, 1] <- 0.7
#loadingValues[4:6, 2] <- 0.7
#LX <- simMatrix(loading, loadingValues)
#latent.cor <- matrix(NA, 2, 2)
#diag(latent.cor) <- 1
#RPH <- symMatrix(latent.cor, 0.5)
#error.cor <- matrix(0, 6, 6)
#diag(error.cor) <- 1
#RTD <- symMatrix(error.cor)
#CFA.Model <- simSetCFA(LX = LX, RPH = RPH, RTD = RTD)

