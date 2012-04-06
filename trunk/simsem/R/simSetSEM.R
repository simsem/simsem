# simSetSEM
# Function -- simsem package
# Description:	This function will create set of matrix that belongs to full SEM model. 
#		The requirement is to specify factor residual correlation matrix, regression coefficient matrix, 
#		factor loading matrix, and measurement error correlation.	
# Function: simSetSEM(..., exo = FALSE)
# Argument:
#	...:	All matrices that belongs to SEM model, see details below.
#	exo:	TRUE if the model includes exogenous elements (X-side in LISREL notation). The default is FALSE (specify endogenous elements only).
# Return: 	SimSet.c containing SEM model (with "SEM" or "SEM.exo" tag)
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: March 10, 2012

simSetSEM <- function(..., exo = FALSE) {
	W <- getKeywords()
	List <- list(...)
	Names <- names(List)
	keywords <- NULL
	if(exo == FALSE) {
		keywords <- list(W$LY, W$RTE, W$VTE, W$VY, W$TY, W$MY, W$BE, W$RPS, W$VPS, W$VE, W$AL, W$ME, W$TE, W$PS) #Length = 14
	} else {
		keywords <- list(W$LY, W$RTE, W$VTE, W$VY, W$TY, W$MY, W$BE, W$RPS, W$VPS, W$VE, W$AL, W$ME, W$TE, W$PS, W$LX, W$RTD, W$VTD, W$VX, W$TX, W$MX, W$GA, W$RPH, W$VPH, W$KA, W$RTH, W$TD, W$PH, W$TH) #Length = 28
	}
	position <- matchKeywords(Names, keywords)
	if(length(position) != length(unique(position))) stop("Some objects were identified more than once.")
	ifelse(contain(1, position), LY <- List[position == 1], stop("No loading object of indicator.Y from factor.ETA in SEM"))
	ny <- nrow(run(LY[[1]]))
	ne <- ncol(run(LY[[1]]))
	if(contain(13, position)) {
		TE <- List[position == 13]
		ifelse(contain(2, position), stop("Error covariance and error correlation cannot be specified at the same time!"), RTE <- list(new("NullSymMatrix")))
		ifelse(contain(3, position), stop("Error covariance and error variance cannot be specified at the same time!"), VTE <- list(new("NullSimVector")))		
		ifelse(contain(4, position), stop("Error covariance and total indicator variance cannot be specified at the same time!"), VY <- list(new("NullSimVector")))
	} else {
		TE <- list(new("NullSymMatrix"))
		ifelse(contain(2, position), RTE <- List[position == 2], stop("No measurement error correlation object between indicator.Y"))
		ifelse(contain(3, position), VTE <- List[position == 3], VTE <- list(new("NullSimVector")))
		ifelse(contain(4, position), VY <- List[position == 4], ifelse(isNullObject(VTE[[1]]), { VY <- list(freeVector(1, ny)); comment(VY[[1]]) <- "default"}, VY <- list(new("NullSimVector"))))
	}
	ifelse(contain(6, position), MY <- List[position == 6], MY <- list(new("NullSimVector")))
	ifelse(contain(5, position), TY <- List[position == 5], ifelse(isNullObject(MY[[1]]), { TY <- list(freeVector(0, ny)); comment(TY[[1]]) <- "default"}, TY <- list(new("NullSimVector"))))
	ifelse(contain(7, position), BE <- List[position == 7], stop("No path coefficient object between factor.ETA"))
	if(contain(14, position)) {
		PS <- List[position == 14]
		ifelse(contain(8, position), stop("Covariance and correlation cannot be specified at the same time!"), RPS <- list(new("NullSymMatrix")))
		ifelse(contain(9, position), stop("Covariance and variance cannot be specified at the same time!"), VPS <- list(new("NullSimVector")))		
		ifelse(contain(10, position), stop("Covariance and total indicator variance cannot be specified at the same time!"), VE <- list(new("NullSimVector")))		
	} else {
		PS <- list(new("NullSymMatrix"))
		ifelse(contain(8, position), RPS <- List[position == 8], stop("No residual correlation object between factor.ETA"))
		ifelse(contain(9, position), VPS <- List[position == 9], VPS <- list(new("NullSimVector")))
		ifelse(contain(10, position), VE <- List[position == 10], ifelse(isNullObject(VPS[[1]]), { VE <- list(constantVector(1, ne)); comment(VE[[1]]) <- "default"}, VE <- list(new("NullSimVector"))))
	}
	ifelse(contain(12, position), ME <- List[position == 12], ME <- list(new("NullSimVector")))
	ifelse(contain(11, position), AL <- List[position == 11], ifelse(isNullObject(ME[[1]]), { AL <- list(constantVector(0, ne)); comment(AL[[1]]) <- "default"}, AL <- list(new("NullSimVector"))))
	Output <- NULL
	if(exo) {
		ifelse(contain(15, position), LX <- List[position == 15], stop("No loading object of indicator.X from factor.KSI in SEM"))
		nx <- nrow(run(LX[[1]]))
		nk <- ncol(run(LX[[1]]))
		if(contain(26, position)) {
			TD <- List[position == 26]
			ifelse(contain(16, position), stop("Error covariance and error correlation cannot be specified at the same time!"), RTD <- list(new("NullSymMatrix")))
			ifelse(contain(17, position), stop("Error covariance and error variance cannot be specified at the same time!"), VTD <- list(new("NullSimVector")))		
			ifelse(contain(18, position), stop("Error covariance and total indicator variance cannot be specified at the same time!"), VX <- list(new("NullSimVector")))
		} else {
			TD <- list(new("NullSymMatrix"))
			ifelse(contain(16, position), RTD <- List[position == 16], stop("No measurement error correlation object between indicator.Y"))
			ifelse(contain(17, position), VTD <- List[position == 17], VTD <- list(new("NullSimVector")))
			ifelse(contain(18, position), VX <- List[position == 18], ifelse(isNullObject(VTD[[1]]), { VX <- list(freeVector(1, nx)); comment(VX[[1]]) <- "default"}, VX <- list(new("NullSimVector"))))
		}
		ifelse(contain(20, position), MX <- List[position == 20], MX <- list(new("NullSimVector")))
		ifelse(contain(19, position), TX <- List[position == 19], ifelse(isNullObject(MX[[1]]), { TX <- list(freeVector(0, nx)); comment(TX[[1]]) <- "default"}, TX <- list(new("NullSimVector"))))
		
		ifelse(contain(21, position), GA <- List[position == 21], stop("No path coefficient object from Factor.KSI to Factor.ETA"))
		if(contain(27, position)) {
			PH <- List[position == 27]
			ifelse(contain(22, position), stop("Covariance and correlation cannot be specified at the same time!"), RPH <- list(new("NullSymMatrix")))
			ifelse(contain(23, position), stop("Covariance and variance cannot be specified at the same time!"), VPH <- list(new("NullSimVector")))				
		} else {
			PH <- list(new("NullSymMatrix"))
			ifelse(contain(22, position), RPH <- List[position == 22], stop("No correlation object between factor.KSI"))
			ifelse(contain(23, position), VPH <- List[position == 23], { VPH <- list(constantVector(1, nk)); comment(VPH[[1]]) <- "default"})
		}
		ifelse(contain(24, position), KA <- List[position == 24], { KA <- list(constantVector(0, nk)); comment(KA[[1]]) <- "default"})
		if(contain(28, position)) {
			ifelse(contain(25, position), stop("TH and RTH cannot be specified at the same time!"), RTH <- list(new("NullSimMatrix")))
			TH <- List[position == 28]
			temp <- run(TH[[1]])
			if(!((nrow(temp) == nx) & (ncol(temp) == ny))) stop("The number of rows is not equal the number of exogenous indicators or the number of columns is not equal the number of endogenous indicators.")
		} else {
			TH <- list(new("NullSimMatrix"))
			if(contain(25, position)) {
				RTH <- List[position == 25]
				temp <- run(RTH[[1]])
				if(!((nrow(temp) == nx) & (ncol(temp) == ny))) stop("The number of rows is not equal the number of exogenous indicators or the number of columns is not equal the number of endogenous indicators.")
			} else {
				RTH.Data <- matrix(0, nx, ny)
				RTH.Labels <- matrix(NA, nx, ny)
				RTH <- list(new("SimMatrix", free=RTH.Data, param=RTH.Labels))
				comment(RTH[[1]]) <- "default"
			}
		}
		Output <- new("SimSet", LY=LY[[1]], TE=TE[[1]], RTE=RTE[[1]], VTE=VTE[[1]], VY=VY[[1]], MY=MY[[1]], TY=TY[[1]], BE=BE[[1]], PS=PS[[1]], RPS=RPS[[1]], VPS=VPS[[1]], VE=VE[[1]], AL=AL[[1]], ME=ME[[1]],
					LX=LX[[1]], TD=TD[[1]], RTD=RTD[[1]], VTD=VTD[[1]], VX=VX[[1]], MX=MX[[1]], TX=TX[[1]], GA=GA[[1]], PH=PH[[1]], RPH=RPH[[1]], VPH=VPH[[1]], KA=KA[[1]], TH=TH[[1]], RTH=RTH[[1]], modelType="SEM.exo")	
	} else {
		Output <- new("SimSet", LY=LY[[1]], TE=TE[[1]], RTE=RTE[[1]], VTE=VTE[[1]], VY=VY[[1]], MY=MY[[1]], TY=TY[[1]], BE=BE[[1]], PS=PS[[1]], RPS=RPS[[1]], VPS=VPS[[1]], VE=VE[[1]], AL=AL[[1]], ME=ME[[1]], modelType="SEM")	
	}
	return(Output)
}

# Details:
#	REQUIRED: LY for factor loading matrix from endogenous factors to Y indicators (need to be SimMatrix.c object). 
#	REQUIRED: TE for measurement error correlation matrix among Y indicators (need to be SymMatrix.c object).
#	REQUIRED: BE for regression coefficient matrix among endogenous factors (need to be SimMatrix.c object). 
#	REQUIRED: PS for residual correlation matrix among endogenous factors (need to be SymMatrix.c object).
#	VTE for measurement error variance of Y indicators (need to be SimVector.c object).
#	VY for total variance of Y indicators (need to be SimVector.c object).
#	NOTE: Either measurement error variance or indicator variance is specified. Both cannot be simultaneously specified.
#	TY for measurement intercepts of Y indicators. (need to be SimVector.c object).
#	MY for overall Y indicator means. (need to be SimVector.c object).
#	NOTE: Either measurement intercept of indicator mean can be specified. Both cannot be specified simultaneously.
#	VPS for residual variance of endogenous factors (need to be SimVector.c object).
#	VE for total endogenous factor variance (need to be SimVector.c object).
#	NOTE: Either total endogenous factor variance or residual endogenous factor variance is specified. Both cannot be simultaneously specified.
#	AL for endogenous factor intercept (need to be SimVector.c object).
#	ME for total mean of endogenous factors (need to be SimVector.c object).
#	NOTE: Either endogenous factor intercept or total mean of endogenous factor is specified. Both cannot be simultaneously specified.

#	NOTE: If users need to specify exogenous variable too.
#	REQUIRED for exo=TRUE: LX for factor loading matrix from exogenous factors to X indicators (need to be SimMatrix.c object). 
#	REQUIRED for exo=TRUE: TD for measurement error correlation matrix among X indicators (need to be SymMatrix.c object).
#	REQUIRED for exo=TRUE: GA for regression coefficient matrix among exogenous factors (need to be SimMatrix.c object). 
#	REQUIRED for exo=TRUE: PH for residual correlation matrix among exogenous factors (need to be SymMatrix.c object).
#	VTD for measurement error variance of X indicators (need to be SimVector.c object).
#	VX for total variance of X indicators (need to be SimVector.c object).
#	NOTE: Either measurement error variance or indicator variance is specified. Both cannot be simultaneously specified.
#	TX for measurement intercepts of Y indicators. (need to be SimVector.c object).
#	MX for overall Y indicator means. (need to be SimVector.c object).
#	NOTE: Either measurement intercept of indicator mean can be specified. Both cannot be specified simultaneously.
#	VPH or VK for total exogenous factor variance (need to be SimVector.c object).
#	KA or MK for total mean of exogenous factors (need to be SimVector.c object).

# Default:
#		1) All indicator variances are equal to 1. Measurement error variances are automatically implied from total indicator variances.
#		2) All measurement error variances are free parameters.
#		3) All indicator means are equal to 0. Indicator intercepts are automatically implied from indicator means.
#		4) All indicator intercepts are free parameters.
#		5) All factor variances are equal to 1.
#		6) All factor variances are fixed.
#		7) All factor means are equal to 0.
#		8) All factor means are fixed.

#Examples:
#u68 <- simUnif(0.6, 0.8)
#loading <- matrix(0, 8, 3)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loading[7:8, 3] <- NA
#loading.start <- matrix("", 8, 3)
#loading.start[1:3, 1] <- 0.7
#loading.start[4:6, 2] <- 0.7
#loading.start[7:8, 3] <- "u68"
#LY <- simMatrix(loading, loading.start)
#TE <- symMatrix(diag(8))
#factor.cor <- diag(3)
#factor.cor[1, 2] <- factor.cor[2, 1] <- NA
#RPS <- symMatrix(factor.cor, 0.5)
#path <- matrix(0, 3, 3)
#path[3, 1:2] <- NA
#path.start <- matrix(0, 3, 3)
#path.start[3, 1] <- "n65"
#path.start[3, 2] <- "u35"
#BE <- simMatrix(path, path.start)
#SEM.model <- simSetSEM(BE=BE, LY=LY, RPS=RPS, TE=TE)
#loading.X <- matrix(0, 6, 2)
#loading.X[1:3, 1] <- NA
#loading.X[4:6, 2] <- NA
#LX <- simMatrix(loading.X, 0.7)
#loading.Y <- matrix(NA, 2, 1)
#LY <- simMatrix(loading.Y, "u68")
#RTD <- symMatrix(diag(6))
#RTE <- symMatrix(diag(2))
#factor.K.cor <- matrix(NA, 2, 2)
#diag(factor.K.cor) <- 1
#RPH <- symMatrix(factor.K.cor, 0.5)
#RPS <- symMatrix(as.matrix(1))
#path.GA <- matrix(NA, 1, 2)
#path.GA.start <- matrix(c("n65", "u35"), ncol=2)
#GA <- simMatrix(path.GA, path.GA.start)
#BE <- simMatrix(as.matrix(0))
#SEM.Exo.model <- simSetSEM(GA=GA, BE=BE, LX=LX, LY=LY, RPH=RPH, RPS=RPS, RTD=RTD, RTE=RTE, exo=TRUE)
