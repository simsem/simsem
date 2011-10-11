# matrix.SEM.object
# Function -- simsem package
# Description:	This function will create set of matrix that belongs to full SEM model. 
#		The requirement is to specify factor residual correlation matrix, regression coefficient matrix, 
#		factor loading matrix, and measurement error correlation.	
# Function: matrix.SEM.object(..., exo = FALSE)
# Argument:
#	...:	All matrices that belongs to SEM model, see details below.
#	exo:	TRUE if the model includes exogenous elements (X-side in LISREL notation). The default is FALSE (specify endogenous elements only).
# Return: 	simMatrixSet.c containing SEM model (with "SEM" or "SEM.exo" tag)
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 6, 2011

matrix.SEM.object <- function(..., exo = FALSE) {
	W <- get.keywords()
	List <- list(...)
	Names <- names(List)
	keywords <- NULL
	if(exo == FALSE) {
		keywords <- list(W$LY, W$TE, W$VTE, W$VY, W$TY, W$MY, W$BE, W$PS, W$VPS, W$VE, W$AL, W$ME)
	} else {
		keywords <- list(W$LY, W$TE, W$VTE, W$VY, W$TY, W$MY, W$BE, W$PS, W$VPS, W$VE, W$AL, W$ME, W$LX, W$TD, W$VTD, W$VX, W$TX, W$MX, W$GA, W$PH, W$VPH, W$KA, W$TH)
	}
	position <- match.keyword(Names, keywords)
	if(length(position) != length(unique(position))) stop("Some objects were identified more than once.")
	ifelse(contain(1, position), LY <- List[position == 1], stop("No loading object of indicator.Y from factor.ETA in SEM"))
	ny <- nrow(run(LY[[1]]))
	ne <- ncol(run(LY[[1]]))
	ifelse(contain(2, position), TE <- List[position == 2], stop("No measurement error correlation object between indicator.Y"))
	ifelse(contain(3, position), VTE <- List[position == 3], VTE <- list(new("nullSimVector")))
	ifelse(contain(4, position), VY <- List[position == 4], ifelse(is.null.object(VTE[[1]]), { VY <- list(constant.vector(1, ny)); comment(VY[[1]]) <- "default"}, VY <- list(new("nullSimVector"))))
	ifelse(contain(6, position), MY <- List[position == 6], MY <- list(new("nullSimVector")))
	ifelse(contain(5, position), TY <- List[position == 5], ifelse(is.null.object(MY[[1]]), { TY <- list(constant.vector(0, ny)); comment(TY[[1]]) <- "default"}, TY <- list(new("nullSimVector"))))
	ifelse(contain(7, position), BE <- List[position == 7], stop("No path coefficient object between factor.ETA"))
	ifelse(contain(8, position), PS <- List[position == 8], stop("No residual correlation object between factor.ETA"))
	ifelse(contain(9, position), VPS <- List[position == 9], VPS <- list(new("nullSimVector")))
	ifelse(contain(10, position), VE <- List[position == 10], ifelse(is.null.object(VPS[[1]]), { VE <- list(constant.vector(1, ne)); comment(VE[[1]]) <- "default"}, VE <- list(new("nullSimVector"))))
	ifelse(contain(12, position), ME <- List[position == 12], ME <- list(new("nullSimVector")))
	ifelse(contain(11, position), AL <- List[position == 11], ifelse(is.null.object(ME[[1]]), { AL <- list(constant.vector(0, ne)); comment(AL[[1]]) <- "default"}, AL <- list(new("nullSimVector"))))
	Output <- NULL
	if(exo) {
		ifelse(contain(13, position), LX <- List[position == 13], stop("No loading object of indicator.X from factor.KSI in SEM"))
		nx <- nrow(run(LX[[1]]))
		nk <- ncol(run(LX[[1]]))
		ifelse(contain(14, position), TD <- List[position == 14], stop("No measurement error correlation object between indicator.Y"))
		ifelse(contain(15, position), VTD <- List[position == 15], VTD <- list(new("nullSimVector")))
		ifelse(contain(16, position), VX <- List[position == 16], ifelse(is.null.object(VTD[[1]]), { VX <- list(constant.vector(1, nx)); comment(VX[[1]]) <- "default"}, VX <- list(new("nullSimVector"))))
		ifelse(contain(18, position), MX <- List[position == 18], MX <- list(new("nullSimVector")))
		ifelse(contain(17, position), TX <- List[position == 17], ifelse(is.null.object(MX[[1]]), { TX <- list(constant.vector(0, nx)); comment(TX[[1]]) <- "default"}, TX <- list(new("nullSimVector"))))
		
		ifelse(contain(19, position), GA <- List[position == 19], stop("No path coefficient object from Factor.KSI to Factor.ETA"))
		ifelse(contain(20, position), PH <- List[position == 20], stop("No correlation object between factor.KSI"))
		ifelse(contain(21, position), VPH <- List[position == 21], { VPH <- list(constant.vector(1, nk)); comment(VPH[[1]]) <- "default"})
		ifelse(contain(22, position), KA <- List[position == 22], { KA <- list(constant.vector(0, nk)); comment(KA[[1]]) <- "default"})
		if(contain(23, position)) {
			TH <- List[position == 23]
			temp <- run(TH[[1]])
			if(!((nrow(temp) == nx) & (ncol(temp) == ny))) stop("The number of rows is not equal the number of exogenous indicators or the number of columns is not equal the number of endogenous indicators.")
		} else {
			TH.Data <- matrix(0, nx, ny)
			TH.Labels <- matrix(NA, nx, ny)
			TH <- list(new("simMatrix", Data=TH.Data, Labels=TH.Labels))
			comment(TH[[1]]) <- "default"
		}
		Output <- new("simMatrixSet", LY=LY[[1]], TE=TE[[1]], VTE=VTE[[1]], VY=VY[[1]], MY=MY[[1]], TY=TY[[1]], BE=BE[[1]], PS=PS[[1]], VPS=VPS[[1]], VE=VE[[1]], AL=AL[[1]], ME=ME[[1]],
					LX=LX[[1]], TD=TD[[1]], VTD=VTD[[1]], VX=VX[[1]], MX=MX[[1]], TX=TX[[1]], GA=GA[[1]], PH=PH[[1]], VPH=VPH[[1]], KA=KA[[1]], TH=TH[[1]], Tag="SEM.exo")	
	} else {
		Output <- new("simMatrixSet", LY=LY[[1]], TE=TE[[1]], VTE=VTE[[1]], VY=VY[[1]], MY=MY[[1]], TY=TY[[1]], BE=BE[[1]], PS=PS[[1]], VPS=VPS[[1]], VE=VE[[1]], AL=AL[[1]], ME=ME[[1]], Tag="SEM")	
	}
	return(Output)
}

# Details:
#	REQUIRED: LY for factor loading matrix from endogenous factors to Y indicators (need to be simMatrix.c object). 
#	REQUIRED: TE for measurement error correlation matrix among Y indicators (need to be symMatrix.c object).
#	REQUIRED: BE for regression coefficient matrix among endogenous factors (need to be simMatrix.c object). 
#	REQUIRED: PS for residual correlation matrix among endogenous factors (need to be symMatrix.c object).
#	VTE for measurement error variance of Y indicators (need to be simVector.c object).
#	VY for total variance of Y indicators (need to be simVector.c object).
#	NOTE: Either measurement error variance or indicator variance is specified. Both cannot be simultaneously specified.
#	TY for measurement intercepts of Y indicators. (need to be simVector.c object).
#	MY for overall Y indicator means. (need to be simVector.c object).
#	NOTE: Either measurement intercept of indicator mean can be specified. Both cannot be specified simultaneously.
#	VPS for residual variance of endogenous factors (need to be simVector.c object).
#	VE for total endogenous factor variance (need to be simVector.c object).
#	NOTE: Either total endogenous factor variance or residual endogenous factor variance is specified. Both cannot be simultaneously specified.
#	AL for endogenous factor intercept (need to be simVector.c object).
#	ME for total mean of endogenous factors (need to be simVector.c object).
#	NOTE: Either endogenous factor intercept or total mean of endogenous factor is specified. Both cannot be simultaneously specified.

#	NOTE: If users need to specify exogenous variable too.
#	REQUIRED for exo=TRUE: LX for factor loading matrix from exogenous factors to X indicators (need to be simMatrix.c object). 
#	REQUIRED for exo=TRUE: TD for measurement error correlation matrix among X indicators (need to be symMatrix.c object).
#	REQUIRED for exo=TRUE: GA for regression coefficient matrix among exogenous factors (need to be simMatrix.c object). 
#	REQUIRED for exo=TRUE: PH for residual correlation matrix among exogenous factors (need to be symMatrix.c object).
#	VTD for measurement error variance of X indicators (need to be simVector.c object).
#	VX for total variance of X indicators (need to be simVector.c object).
#	NOTE: Either measurement error variance or indicator variance is specified. Both cannot be simultaneously specified.
#	TX for measurement intercepts of Y indicators. (need to be simVector.c object).
#	MX for overall Y indicator means. (need to be simVector.c object).
#	NOTE: Either measurement intercept of indicator mean can be specified. Both cannot be specified simultaneously.
#	VPH or VK for total exogenous factor variance (need to be simVector.c object).
#	KA or MK for total mean of exogenous factors (need to be simVector.c object).

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
#u68 <- runif.object(0.6, 0.8)
#loading <- matrix(0, 8, 3)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loading[7:8, 3] <- NA
#loading.start <- matrix("", 8, 3)
#loading.start[1:3, 1] <- 0.7
#loading.start[4:6, 2] <- 0.7
#loading.start[7:8, 3] <- "u68"
#LY <- matrix.object(loading, loading.start)
#TE <- sym.matrix.object(diag(8))
#factor.cor <- diag(3)
#factor.cor[1, 2] <- factor.cor[2, 1] <- NA
#PS <- sym.matrix.object(factor.cor, 0.5)
#path <- matrix(0, 3, 3)
#path[3, 1:2] <- NA
#path.start <- matrix(0, 3, 3)
#path.start[3, 1] <- "n65"
#path.start[3, 2] <- "u35"
#BE <- matrix.object(path, path.start)
#SEM.model <- matrix.SEM.object(BE=BE, LY=LY, PS=PS, TE=TE)
#loading.X <- matrix(0, 6, 2)
#loading.X[1:3, 1] <- NA
#loading.X[4:6, 2] <- NA
#LX <- matrix.object(loading.X, 0.7)
#loading.Y <- matrix(NA, 2, 1)
#LY <- matrix.object(loading.Y, "u68")
#TD <- sym.matrix.object(diag(6))
#TE <- sym.matrix.object(diag(2))
#factor.K.cor <- matrix(NA, 2, 2)
#diag(factor.K.cor) <- 1
#PH <- sym.matrix.object(factor.K.cor, 0.5)
#PS <- sym.matrix.object(as.matrix(1))
#path.GA <- matrix(NA, 1, 2)
#path.GA.start <- matrix(c("n65", "u35"), ncol=2)
#GA <- matrix.object(path.GA, path.GA.start)
#BE <- matrix.object(as.matrix(0))
#SEM.Exo.model <- matrix.SEM.object(GA=GA, BE=BE, LX=LX, LY=LY, PH=PH, PS=PS, TD=TD, TE=TE, exo=TRUE)
