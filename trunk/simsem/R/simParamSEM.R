# simParamSEM
# Function -- simsem package
# Create a set of free parameter matrices that belongs to SEM model.
# Argument:
#	...:	All matrices that belongs to SEM model, see details below.
#	exo:	TRUE if the model includes exogenous elements (X-side in LISREL notation). The default is FALSE (specify endogenous elements only).
# Return: 	SimParam class containing SEM model (with "SEM" or "SEM.exo" tag)
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: April 9, 2012

simParamSEM <- function(..., exo = FALSE) {
	W <- getKeywords()
	List <- list(...)
	Names <- names(List)
	keywords <- NULL
	if(exo == FALSE) {
		keywords <- list(W$LY, W$TY, W$BE, W$AL, W$TE, W$PS) #Length = 6
	} else {
		keywords <- list(W$LY, W$TY, W$BE, W$AL, W$TE, W$PS, W$LX, W$TX, W$GA, W$KA, W$TD, W$PH, W$TH) #Length = 13
	}
	position <- matchKeywords(Names, keywords)
	if(length(position) != length(unique(position))) stop("Some objects were identified more than once.")
	if(exo) {
		ifelse(9 %in% position, GA <- List[position == 9][[1]], stop("No path coefficient object between factor.ETA and factor.KSI"))
		ne <- nrow(GA)
		nk <- ncol(GA)
		ifelse(3 %in% position, BE <- List[position == 3][[1]], BE <- matrix(0, ne, ne))
		ifelse(4 %in% position, AL <- List[position == 4][[1]], AL <- rep(NA, ne))
		ifelse(6 %in% position, PS <- List[position == 6][[1]], PS <- diag(NA, ne))
		ifelse(10 %in% position, KA <- List[position == 10][[1]], KA <- rep(NA, nk))
		ifelse(12 %in% position, PH <- List[position == 12][[1]], PH <- matrix(NA, nk, nk))
		ifelse(1 %in% position, LY <- List[position == 1][[1]], stop("No loading object of indicator.Y from factor.ETA in SEM"))
		ny <- nrow(LY)
		ifelse(2 %in% position, TY <- List[position == 2][[1]], TY <- rep(NA, ny))
		ifelse(5 %in% position, TE <- List[position == 5][[1]], TE <- diag(NA, ny))
		ifelse(7 %in% position, LX <- List[position == 7][[1]], stop("No loading object of indicator.X from factor.KSI in SEM"))
		nx <- nrow(LX)
		ifelse(8 %in% position, TX <- List[position == 8][[1]], TX <- rep(NA, nx))
		ifelse(11 %in% position, TD <- List[position == 11][[1]], TD <- diag(NA, nx))
		ifelse(13 %in% position, TH <- List[position == 13][[1]], TH <- matrix(0, nx, ny))
		Output <- new("SimParam", BE=BE, PS=PS, AL=AL, LY=LY, TY=TY, TE=TE, GA=GA, PH=PH, KA=KA, LX=LX, TX=TX, TD=TD, TH=TH, modelType="SEM.exo")	
	} else {
		ifelse(3 %in% position, BE <- List[position == 3][[1]], stop("No path coefficient object between factor.ETA"))
		ne <- nrow(BE)
		ifelse(4 %in% position, AL <- List[position == 4][[1]], AL <- rep(0, ne))
		PS <- NULL
		if(6 %in% position) { 
			PS <- List[position == 6][[1]]
		} else {
			PS <- diag(1, ne)
			set <- findRecursiveSet(BE)
			PS[set[[1]], set[[1]]] <- NA
			diag(PS) <- 1
		}
		ifelse(1 %in% position, LY <- List[position == 1][[1]], stop("No loading object of indicator.Y from factor.ETA in SEM"))
		ny <- nrow(LY)
		ifelse(2 %in% position, TY <- List[position == 2][[1]], TY <- rep(NA, ny))
		ifelse(5 %in% position, TE <- List[position == 5][[1]], TE <- diag(NA, ny))
		Output <- new("SimParam", BE=BE, PS=PS, AL=AL, LY=LY, TY=TY, TE=TE, modelType="SEM")
	}
	return(Output)
}

# Details:
#	REQUIRED: LY for factor loading matrix from endogenous factors to Y indicators 
#	REQUIRED (not required if exo=TRUE): BE for regression coefficient matrix among endogenous factors  
#	TE for measurement error correlation matrix among Y indicators
#	PS for residual covariance matrix among endogenous factors
#	TY for measurement intercepts of Y indicators. 
#	AL for endogenous factor intercept 

#	NOTE: If users need to specify exogenous variable too.
#	REQUIRED for exo=TRUE: LX for factor loading matrix from exogenous factors to X indicators  
#	REQUIRED for exo=TRUE: GA for regression coefficient matrix of exogenous factors onto endogenous factors
#	TD for measurement error covariance matrix among X indicators 
#	PH for residual covariance matrix among exogenous factors 
#	TX for measurement intercepts of X indicators. 
#	KA or MK for total mean of exogenous factors 
#	TH for measurement covariance between X indicators and Y indicators

# Default:
#	A. If exo=FALSE
# 		1) If residual factor covariance is not specified, then (a) all factor variances are free, (b) all exogenous covariances are free, (c) all endogenous covariances are fixed.
#		2) If factor means vector is not specified, then the factor means are free.
#		3) If error covariance matrix is not specified, the default is to estimate all error variances and not estimate error covariances.
#		4) If measurement intercept vector is not specified, the default is to estimate all measurement intercepts.
#	B. If exo=TRUE
# 		1) If PS is not specified, then (a) all factor variances are free, (b) all endogenous covariances are fixed.
#		2) If BE is not specified, then BE is specified as zero matrix
#		3) If factor means (KA or AL) are not specified, all indictor means are free.
#		4) If PH is not specified, then PH is the matrix with free parameters in every element.
#		5) If error covariance matrix (TE, TD, or TH) is not specified, the default is to estimate all error variances and not estimate error covariances.
#		6) If measurement intercept vector (TX or TY) is not specified, the default is to estimate all measurement intercepts.

#Examples:
#loading <- matrix(0, 8, 3)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loading[7:8, 3] <- NA
#path <- matrix(0, 3, 3)
#path[3, 1:2] <- NA
#SEM.model <- simParamSEM(BE=path, LY=loading)
#loading.X <- matrix(0, 6, 2)
#loading.X[1:3, 1] <- NA
#loading.X[4:6, 2] <- NA
#loading.Y <- matrix(NA, 2, 1)
#path.GA <- matrix(NA, 1, 2)
#BE <- as.matrix(0)
#SEM.Exo.model <- simParamSEM(GA=path.GA, BE=BE, LX=loading.X, LY=loading.Y, exo=TRUE)
