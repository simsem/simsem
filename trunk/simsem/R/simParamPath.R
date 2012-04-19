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

simParamPath <- function(..., exo = FALSE) {
	W <- getKeywords()
	List <- list(...)
	Names <- names(List)
	keywords <- NULL
	if(exo == FALSE) {
		keywords <- list(W$BE, W$AL, W$PS) # Length = 3
	} else {
		keywords <- list(W$BE, W$AL, W$PS, W$GA, W$KA, W$PH) # Length = 6
	}
	position <- matchKeywords(Names, keywords)
	if(length(position) != length(unique(position))) stop("Some objects were identified more than once.")
	if(exo) {
		ifelse(4 %in% position, GA <- List[position == 4][[1]], stop("No path coefficient object between factor.ETA and factor.KSI"))
		ne <- nrow(GA)
		nk <- ncol(GA)
		ifelse(1 %in% position, BE <- List[position == 1][[1]], BE <- matrix(0, ne, ne))
		ifelse(2 %in% position, AL <- List[position == 2][[1]], AL <- rep(NA, ne))
		ifelse(3 %in% position, PS <- List[position == 3][[1]], PS <- diag(NA, ne))
		ifelse(5 %in% position, KA <- List[position == 5][[1]], KA <- rep(NA, nk))
		ifelse(6 %in% position, PH <- List[position == 6][[1]], PH <- matrix(NA, nk, nk))
		Output <- new("SimParam", BE=BE, PS=PS, AL=AL, GA=GA, PH=PH, KA=KA, modelType="Path.exo")	
	} else {
		ifelse(1 %in% position, BE <- List[position == 1][[1]], stop("No path coefficient object between factor.ETA"))
		ne <- nrow(BE)
		ifelse(2 %in% position, AL <- List[position == 2][[1]], AL <- rep(NA, ne))
		PS <- NULL
		if(3 %in% position) { 
			PS <- List[position == 3][[1]]
		} else {
			PS <- diag(NA, ne)
			set <- findRecursiveSet(BE)
			PS[set[[1]], set[[1]]] <- NA
		}
		Output <- new("SimParam", BE=BE, PS=PS, AL=AL, modelType="Path")
	}
	return(Output)
}

# Details:
#	REQUIRED (not required if exo=TRUE): BE for regression coefficient matrix
#	PS for residual covariance 
#	AL for indicator intercept 

#	NOTE: If users need to specify exogenous variable too.
#	REQUIRED for exo=TRUE: GA for regression coefficient matrix from exogenous variable to endogenous variable 
#	PH for covariance between exogenous variables
#	KA or MK for means of exogenous variables

# Default:
#	A. If exo=FALSE
# 		1) If residual covariance is not specified, then (a) all indicator variances are free, (b) all exogenous covariances are free, (c) all endogenous covariances are fixed.
#		2) If indicator means vector is not specified, then the indicator means are free.
#	B. If exo=TRUE
# 		1) If PS is not specified, then (a) all indicator variances are free, (b) all endogenous covariances are fixed.
#		2) If BE is not specified, then BE is specified as zero matrix
#		3) If indicator means (KA or AL) are not specified, all indictor means are free.
#		4) If PH is not specified, then PH is the matrix with free parameters in every element.

#Example:
#BE <- matrix(0, 4, 4)
#BE[3, 1:2] <- NA
#BE[4, 3] <- NA
#Path.Model <- simParamPath(BE = BE)
#GA <- matrix(NA, 2, 3)
#Path.Exo.Model <- simParamPath(GA = GA, exo=TRUE)
