# simMisspecPath
# Function -- simsem package
# Create a set of matrices that belongs to path analysis misspecification model.
# Function: simMisspecPath(...)
# Argument:
#	...:	The matrices that users wish to specify the misspecification
#	exo:	Whether specifying the exogenous side
# Return: 	SimMisspec class (with "Path" or "Path.exo" tag)
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: March 10, 2012

simMisspecPath <- function(..., exo = FALSE) {
	W <- get.keywords()
	List <- list(...)
	Names <- names(List)
	keywords <- NULL
	if(exo == FALSE) {
		keywords <- list(W$BE, W$RPS, W$VPS, W$VE, W$AL, W$ME, W$PS) # Length = 7
	} else {
		keywords <- list(W$BE, W$RPS, W$VPS, W$VE, W$AL, W$ME, W$PS, W$GA, W$RPH, W$VPH, W$KA, W$PH) # Length = 12
	}
	position <- match.keyword(Names, keywords)
	if(length(position) != length(unique(position))) stop("Some objects were identified more than once.")
	ifelse(contain(1, position), BE <- List[position == 1], BE <- list(new("NullSimMatrix")))
	if(contain(7, position)) {
		PS <- List[position == 7]
		ifelse(contain(2, position), stop("Covariance and correlation cannot be specified at the same time!"), RPS <- list(new("NullSymMatrix")))
		ifelse(contain(3, position), stop("Covariance and variance cannot be specified at the same time!"), VPS <- list(new("NullSimVector")))		
		ifelse(contain(4, position), stop("Covariance and total indicator variance cannot be specified at the same time!"), VE <- list(new("NullSimVector")))		
	} else {
		PS <- list(new("NullSymMatrix"))
		ifelse(contain(2, position), RPS <- List[position == 2], RPS <- list(new("NullSymMatrix")))
		ifelse(contain(3, position), VPS <- List[position == 3], VPS <- list(new("NullSimVector")))
		ifelse(contain(4, position), VE <- List[position == 4], VE <- list(new("NullSimVector")))
	}
	if(!is.null.object(VPS[[1]]) & !is.null.object(VE[[1]])) stop("Please assign either VPS or VE, not both")
	ifelse(contain(6, position), ME <- List[position == 6], ME <- list(new("NullSimVector")))
	ifelse(contain(5, position), AL <- List[position == 5], AL <- list(new("NullSimVector")))
	if(!is.null.object(ME[[1]]) & !is.null.object(AL[[1]])) stop("Please assign either ME or AL, not both")
	Output <- NULL
	if(exo) {
		ifelse(contain(8, position), GA <- List[position == 8], GA <- list(new("NullSimMatrix")))
		if(contain(12, position)) {
			PH <- List[position == 12]
			ifelse(contain(9, position), stop("Covariance and correlation cannot be specified at the same time!"), RPH <- list(new("NullSymMatrix")))
			ifelse(contain(10, position), stop("Covariance and variance cannot be specified at the same time!"), VPH <- list(new("NullSimVector")))				
		} else {
			PH <- list(new("NullSymMatrix"))
			ifelse(contain(9, position), RPH <- List[position == 9], RPH <- list(new("NullSymMatrix")))
			ifelse(contain(10, position), VPH <- List[position == 10], VPH <- list(new("NullSimVector")))
		}
		ifelse(contain(11, position), KA <- List[position == 11], KA <- list(new("NullSimVector")))
		Output <- new("SimMisspec", BE=BE[[1]], PS=PS[[1]], RPS=RPS[[1]], VPS=VPS[[1]], VE=VE[[1]], AL=AL[[1]], ME=ME[[1]], GA=GA[[1]], PH=PH[[1]], RPH=RPH[[1]], VPH=VPH[[1]], KA=KA[[1]], modelType="Path.exo")	
	} else {
		Output <- new("SimMisspec", BE=BE[[1]], PS=PS[[1]], RPS=RPS[[1]], VPS=VPS[[1]], VE=VE[[1]], AL=AL[[1]], ME=ME[[1]], modelType="Path")
	}
	return(Output)
}
