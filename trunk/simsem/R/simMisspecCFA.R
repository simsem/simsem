# simMisspecCFA
# Function -- simsem package
# Create a set of matrices that belongs to CFA misspecification model.
# Function: simMisspecCFA(...)
# Argument:
#	...:	The matrices that users wish to specify the misspecification
# Return: 	SimMisspec class (with "CFA" tag)
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: March 10, 2012

simMisspecCFA <- function(...) { 
	W <- get.keywords()
	List <- list(...)
	Names <- names(List)
	keywords <- list(W$loading, W$errorCor, W$facCor, W$errorVar, W$indicatorVar, W$intercept, W$facMean, W$indicatorMean, W$facVar, W$errorCov, W$facCov)
	position <- match.keyword(Names, keywords)
	if(length(position) != length(unique(position))) stop("Some objects were identified more than once.")
	ifelse(contain(1, position), LY <- List[position == 1], LY <- list(new("NullSimMatrix")))
	
	if(contain(10, position)) {
		TE <- List[position == 10]
		ifelse(contain(2, position), stop("Error covariance and error correlation cannot be specified at the same time!"), RTE <- list(new("NullSymMatrix")))
		ifelse(contain(4, position), stop("Error covariance and error variance cannot be specified at the same time!"), VTE <- list(new("NullSimVector")))		
		ifelse(contain(5, position), stop("Error covariance and total indicator variance cannot be specified at the same time!"), VY <- list(new("NullSimVector")))
	} else {
		TE <- list(new("NullSymMatrix"))
		ifelse(contain(2, position), RTE <- List[position == 2], RTE <- list(new("NullSymMatrix")))
		ifelse(contain(4, position), VTE <- List[position == 4], VTE <- list(new("NullSimVector")))
		ifelse(contain(5, position), VY <- List[position == 5], VY <- list(new("NullSimVector")))
		if(!isNullObject(VTE[[1]]) & !isNullObject(VY[[1]])) stop("Please assign either VTE or VY, not both")
	}
	if(contain(11, position)) {
		PS <- List[position == 11]
		ifelse(contain(3, position), stop("Factor covariance and factor correlation cannot be specified at the same time!"), RPS <- list(new("NullSymMatrix")))
		ifelse(contain(9, position), stop("Factor covariance and factor variance cannot be specified at the same time!"), VE <- list(new("NullSimVector")))		
	} else {
		PS <- list(new("NullSymMatrix"))
		ifelse(contain(3, position), RPS <- List[position == 3], RPS <- list(new("NullSymMatrix")))
		ifelse(contain(9, position), VE <- List[position == 9], VE <- list(new("NullSimVector")))
	}
	ifelse(contain(8, position), MY <- List[position == 8], MY <- list(new("NullSimVector")))
	ifelse(contain(6, position), TY <- List[position == 6], TY <- list(new("NullSimVector")))
	if(!isNullObject(MY[[1]]) & !isNullObject(TY[[1]])) stop("Please assign either MY or TY, not both")
	ifelse(contain(7, position), ME <- List[position == 7], ME <- list(new("NullSimVector")))
	Output <- new("SimMisspec", LY=LY[[1]], PS=PS[[1]], RPS=RPS[[1]], TE=TE[[1]], RTE=RTE[[1]], VE=VE[[1]], VPS=VE[[1]], VTE=VTE[[1]], VY=VY[[1]], TY=TY[[1]], MY=MY[[1]], ME=ME[[1]], AL=ME[[1]], modelType="CFA")
	return(Output)
}
