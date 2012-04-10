# simParamCFA
# Function -- simsem package
# Create a set of free parameter matrices that belongs to CFA model.
# Function: simParamCFA(...)
# Argument:
#	...:	All matrices that belongs to CFA model, see details below.
# Return: 	SimFreeParam class containing CFA model (with "CFA" tag)
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: April 9, 2012

simParamCFA <- function(...) { 
	W <- getKeywords()
	List <- list(...)
	Names <- names(List)
	keywords <- list(W$loading, W$intercept, W$facMean, W$errorCov, W$facCov) # 5 total
	position <- matchKeywords(Names, keywords)
	if(length(position) != length(unique(position))) stop("Some objects were identified more than once.")
	ifelse(contain(1, position), LY <- List[position == 1][[1]], stop("No loading object in CFA"))
	ni <- nrow(LY)
	nk <- ncol(LY)
	ifelse(contain(4, position), TE <- List[position == 4][[1]], TE <- diag(NA, ni))
	ifelse(contain(5, position), PS <- List[position == 5][[1]], {PS <- matrix(NA, nk, nk); diag(PS) <- 1})
	ifelse(contain(2, position), TY <- List[position == 2][[1]], TY <- rep(NA, ni))
	ifelse(contain(3, position), AL <- List[position == 3][[1]], AL <- rep(1, nk))
	Output <- new("SimFreeParam", LY=LY, PS=PS, TE=TE, TY=TY, AL=AL, modelType="CFA")
	return(Output)
}

#Details of the names of elements in CFA models:
#	###CFA object can be either specified in X or Y side.
#	REQUIRED: LX or LY for factor loading matrix (need to be SimMatrix.c object). 
#	TD or TE for error covariance matrix.
#	PH or PS for factor covariance matrix
#	TX or TY for measurement intercepts. 
#	KA, AL, MK, or ME for factor means.

#DEFAULT:
# 	1) The scale-identification default of this model is fixed factor method (factor variances = 1 and factor means = 0).
#	2) If error covariance matrix is not specified, the default is to estimate all error variances and not estimate error covariances.
#	3) If factor covariance matrix is not specified, the default is to fix all factor covariance
#	4) If factor mean vector is not specified, the default is to fix all factor means to 0.
#	5) If measurement intercept vector is not specified, the default is to estimate all measurement intercepts.


#Examples:
#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#CFA.Model <- simParamCFA(LX = loading)

