# simSetPath
# Function -- simsem package
# Description:	This function will create set of matrix that belongs to path analysis model. 
# 		The requirement is to specify indicator correlation and regression coefficient matrix.	
# Function: simSetPath(..., exo = FALSE)
# Argument:
#	...:	All matrices that belongs to Path analysis model, see details below.
#	exo:	TRUE if the model includes exogenous elements (X-side in LISREL notation). The default is FALSE (specify endogenous elements only).
# Return: 	SimSet.c containing Path model (with "Path" or "Path.exo" tag)
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: March 10, 2012

simSetPath <- function(..., exo = FALSE) {
	W <- getKeywords()
	List <- list(...)
	Names <- names(List)
	keywords <- NULL
	if(exo == FALSE) {
		keywords <- list(W$BE, W$RPS, W$VPS, W$VE, W$AL, W$ME, W$PS) # Length = 7
	} else {
		keywords <- list(W$BE, W$RPS, W$VPS, W$VE, W$AL, W$ME, W$PS, W$GA, W$RPH, W$VPH, W$KA, W$PH) # Length = 12
	}
	position <- matchKeywords(Names, keywords)
	if(length(position) != length(unique(position))) stop("Some objects were identified more than once.")
	ifelse(contain(1, position), BE <- List[position == 1], stop("No path coefficient object between factor.ETA"))
	ne <- ncol(run(BE[[1]]))
	if(contain(7, position)) {
		PS <- List[position == 7]
		ifelse(contain(2, position), stop("Covariance and correlation cannot be specified at the same time!"), RPS <- list(new("NullSymMatrix")))
		ifelse(contain(3, position), stop("Covariance and variance cannot be specified at the same time!"), VPS <- list(new("NullSimVector")))		
		ifelse(contain(4, position), stop("Covariance and total indicator variance cannot be specified at the same time!"), VE <- list(new("NullSimVector")))		
	} else {
		PS <- list(new("NullSymMatrix"))
		ifelse(contain(2, position), RPS <- List[position == 2], stop("No residual correlation object between factor.ETA"))
		ifelse(contain(3, position), VPS <- List[position == 3], VPS <- list(new("NullSimVector")))
		ifelse(contain(4, position), VE <- List[position == 4], ifelse(isNullObject(VPS[[1]]), { VE <- list(freeVector(1, ne)); comment(VE[[1]]) <- "default"}, VE <- list(new("NullSimVector"))))
	}
	ifelse(contain(6, position), ME <- List[position == 6], ME <- list(new("NullSimVector")))
	ifelse(contain(5, position), AL <- List[position == 5], ifelse(isNullObject(ME[[1]]), { AL <- list(freeVector(0, ne)); comment(AL[[1]]) <- "default"}, AL <- list(new("NullSimVector"))))
	Output <- NULL
	if(exo) {
		ifelse(contain(8, position), GA <- List[position == 8], stop("No path coefficient object from Factor.KSI to Factor.ETA"))
		nk <- ncol(run(GA[[1]]))
		if(contain(12, position)) {
			PH <- List[position == 12]
			ifelse(contain(9, position), stop("Covariance and correlation cannot be specified at the same time!"), RPH <- list(new("NullSymMatrix")))
			ifelse(contain(10, position), stop("Covariance and variance cannot be specified at the same time!"), VPH <- list(new("NullSimVector")))				
		} else {
			PH <- list(new("NullSymMatrix"))
			ifelse(contain(9, position), RPH <- List[position == 9], stop("No correlation object between factor.KSI"))
			ifelse(contain(10, position), VPH <- List[position == 10], { VPH <- list(freeVector(1, nk)); comment(VPH[[1]]) <- "default"})
		}
		ifelse(contain(11, position), KA <- List[position == 11], { KA <- list(freeVector(0, nk)); comment(KA[[1]]) <- "default"})
		Output <- new("SimSet", BE=BE[[1]], PS=PS[[1]], RPS=RPS[[1]], VPS=VPS[[1]], VE=VE[[1]], AL=AL[[1]], ME=ME[[1]], GA=GA[[1]], PH=PH[[1]], RPH=RPH[[1]], VPH=VPH[[1]], KA=KA[[1]], modelType="Path.exo")	
	} else {
		Output <- new("SimSet", BE=BE[[1]], PS=PS[[1]], RPS=RPS[[1]], VPS=VPS[[1]], VE=VE[[1]], AL=AL[[1]], ME=ME[[1]], modelType="Path")
	}
	return(Output)
}

# Details:
#	REQUIRED: BE for regression coefficient matrix (need to be SimMatrix.c object). 
#	REQUIRED: RPS for residual correlation matrix (need to be SymMatrix.c object).
#	VPS for residual indicator variance (need to be SimVector.c object).
#	VE for total indicator variance (need to be SimVector.c object).
#	NOTE: Either total indicator variance or residual indicator variance is specified. Both cannot be simultaneously specified.
#	AL for indicator intercept (need to be SimVector.c object).
#	ME for indicator total mean (need to be SimVector.c object).
#	NOTE: Either indicator intercept or indicator total mean is specified. Both cannot be simultaneously specified.


#	VPS for residual indicator variance (need to be SimVector.c object).
#	VE for total indicator variance (need to be SimVector.c object).
#	NOTE: Either total indicator variance or residual indicator variance is specified. Both cannot be simultaneously specified.
#	AL for indicator intercept (need to be SimVector.c object).
#	ME for indicator total mean (need to be SimVector.c object).
#	NOTE: Either indicator intercept or indicator total mean is specified. Both cannot be simultaneously specified.

#	NOTE: If users need to specify exogenous variable too.
#	REQUIRED for exo=TRUE: GA for regression coefficient matrix from exogenous variable to endogenous variable (need to be SimMatrix.c object).  
#	REQUIRED for exo=TRUE: RPH for exogenous factor correlation (need to be SymMatrix.c object).
#	VPH or VK for exogenous variable variance (need to be SimVector.c object).
#	KA or MK for exogenous variable mean (need to be SimVector.c object).

# Default:
# 		1) All indicator variances are equal to 1. Residual variances are automatically implied from total indicator variances.
#		2) All residual variances are free parameters.
#		3) All indicator means are equal to 0. Intercepts are automatically implied from total indicator mean.
#		4) All indicator intercepts are free parameters.

#Example:
#u35 <- simUnif(0.3, 0.5)
#u57 <- simUnif(0.5, 0.7)
#u1 <- simUnif(-0.1, 0.1)
#n31 <- simNorm(0.3, 0.1)
#path.BE <- matrix(0, 4, 4)
#path.BE[3, 1:2] <- NA
#path.BE[4, 3] <- NA
#starting.BE <- matrix("", 4, 4)
#starting.BE[3, 1:2] <- "u35"
#starting.BE[4, 3] <- "u57"
#BE <- simMatrix(path.BE, starting.BE)
#residual.error <- diag(4)
#residual.error[1,2] <- residual.error[2,1] <- NA
#RPS <- symMatrix(residual.error, "n31")
#Path.Model <- simSetPath(RPS = RPS, BE = BE)
#u35 <- simUnif(0.3, 0.5)
#u57 <- simUnif(0.5, 0.7)
#u1 <- simUnif(-0.1, 0.1)
#n31 <- simNorm(0.3, 0.1)
#path.GA <- matrix(0, 2, 2)
#path.GA[1, 1:2] <- NA
#GA <- simMatrix(path.GA, "u35")
#path.BE <- matrix(0, 2, 2)
#path.BE[2, 1] <- NA
#BE <- simMatrix(path.BE, "u57")
#exo.cor <- matrix(NA, 2, 2)
#diag(exo.cor) <- 1
#RPH <- symMatrix(exo.cor, "n31")
#RPS <- symMatrix(diag(2))
#Path.Exo.Model <- simSetPath(RPS = RPS, BE = BE, RPH = RPH, GA = GA, exo=TRUE)
