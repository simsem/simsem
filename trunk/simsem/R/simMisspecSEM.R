# simMisspecSEM
# Function -- simsem package
# Create a set of matrices that belongs to SEM misspecification model.
# Function: simMisspecSEM(...)
# Argument:
#	...:	The matrices that users wish to specify the misspecification
#	exo:	Whether specifying the exogenous side
# Return: 	SimMisspec class (with "SEM" or "SEM.exo" tag)
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: March 10, 2012

simMisspecSEM <- function(..., exo = FALSE) {
	W <- get.keywords()
	List <- list(...)
	Names <- names(List)
	keywords <- NULL
	if(exo == FALSE) {
		keywords <- list(W$LY, W$RTE, W$VTE, W$VY, W$TY, W$MY, W$BE, W$RPS, W$VPS, W$VE, W$AL, W$ME, W$TE, W$PS) #Length = 14
	} else {
		keywords <- list(W$LY, W$RTE, W$VTE, W$VY, W$TY, W$MY, W$BE, W$RPS, W$VPS, W$VE, W$AL, W$ME, W$TE, W$PS, W$LX, W$RTD, W$VTD, W$VX, W$TX, W$MX, W$GA, W$RPH, W$VPH, W$KA, W$RTH, W$TD, W$PH, W$TH) #Length = 28
	}
	position <- match.keyword(Names, keywords)
	if(length(position) != length(unique(position))) stop("Some objects were identified more than once.")
	ifelse(contain(1, position), LY <- List[position == 1], LY <- list(new("NullSimMatrix")))
	if(contain(13, position)) {
		TE <- List[position == 13]
		ifelse(contain(2, position), stop("Error covariance and error correlation cannot be specified at the same time!"), RTE <- list(new("NullSymMatrix")))
		ifelse(contain(3, position), stop("Error covariance and error variance cannot be specified at the same time!"), VTE <- list(new("NullSimVector")))		
		ifelse(contain(4, position), stop("Error covariance and total indicator variance cannot be specified at the same time!"), VY <- list(new("NullSimVector")))
	} else {
		TE <- list(new("NullSymMatrix"))
		ifelse(contain(2, position), RTE <- List[position == 2], RTE <- list(new("NullSymMatrix")))
		ifelse(contain(3, position), VTE <- List[position == 3], VTE <- list(new("NullSimVector")))
		ifelse(contain(4, position), VY <- List[position == 4], VY <- list(new("NullSimVector")))
		if(!is.null.object(VTE[[1]]) & !is.null.object(VY[[1]])) stop("Please assign either VTE or VY, not both")
	}
	ifelse(contain(6, position), MY <- List[position == 6], MY <- list(new("NullSimVector")))
	ifelse(contain(5, position), TY <- List[position == 5], TY <- list(new("NullSimVector")))
	if(!is.null.object(MY[[1]]) & !is.null.object(TY[[1]])) stop("Please assign either MY or TY, not both")
	ifelse(contain(7, position), BE <- List[position == 7], BE <- list(new("NullSimMatrix")))
	if(contain(14, position)) {
		PS <- List[position == 14]
		ifelse(contain(8, position), stop("Covariance and correlation cannot be specified at the same time!"), RPS <- list(new("NullSymMatrix")))
		ifelse(contain(9, position), stop("Covariance and variance cannot be specified at the same time!"), VPS <- list(new("NullSimVector")))		
		ifelse(contain(10, position), stop("Covariance and total indicator variance cannot be specified at the same time!"), VE <- list(new("NullSimVector")))		
	} else {
		PS <- list(new("NullSymMatrix"))
		ifelse(contain(8, position), RPS <- List[position == 8], RPS <- list(new("NullSymMatrix")))
		ifelse(contain(9, position), VPS <- List[position == 9], VPS <- list(new("NullSimVector")))
		ifelse(contain(10, position), VE <- List[position == 10], VE <- list(new("NullSimVector")))
		if(!is.null.object(VPS[[1]]) & !is.null.object(VE[[1]])) stop("Please assign either VPS or VE, not both")
	}
	ifelse(contain(12, position), ME <- List[position == 12], ME <- list(new("NullSimVector")))
	ifelse(contain(11, position), AL <- List[position == 11], AL <- list(new("NullSimVector")))
	if(!is.null.object(ME[[1]]) & !is.null.object(AL[[1]])) stop("Please assign either ME or AL, not both")
	Output <- NULL
	if(exo) {
		ifelse(contain(15, position), LX <- List[position == 15], LX <- list(new("NullSimMatrix")))
		if(contain(26, position)) {
			TE <- List[position == 26]
			ifelse(contain(16, position), stop("Error covariance and error correlation cannot be specified at the same time!"), RTD <- list(new("NullSymMatrix")))
			ifelse(contain(17, position), stop("Error covariance and error variance cannot be specified at the same time!"), VTD <- list(new("NullSimVector")))		
			ifelse(contain(18, position), stop("Error covariance and total indicator variance cannot be specified at the same time!"), VX <- list(new("NullSimVector")))
		} else {
			TE <- list(new("NullSymMatrix"))
			ifelse(contain(16, position), RTD <- List[position == 16], RTD <- list(new("NullSymMatrix")))
			ifelse(contain(17, position), VTD <- List[position == 17], VTD <- list(new("NullSimVector")))
			ifelse(contain(18, position), VX <- List[position == 18], VX <- list(new("NullSimVector")))
			if(!is.null.object(VTD[[1]]) & !is.null.object(VX[[1]])) stop("Please assign either VTD or VX, not both")
		}
		ifelse(contain(20, position), MX <- List[position == 20], MX <- list(new("NullSimVector")))
		ifelse(contain(19, position), TX <- List[position == 19], TX <- list(new("NullSimVector")))
		if(!is.null.object(MX[[1]]) & !is.null.object(TX[[1]])) stop("Please assign either MX or TX, not both")
		ifelse(contain(21, position), GA <- List[position == 21], GA <- list(new("NullSimMatrix")))
		if(contain(27, position)) {
			PH <- List[position == 27]
			ifelse(contain(22, position), stop("Covariance and correlation cannot be specified at the same time!"), RPH <- list(new("NullSymMatrix")))
			ifelse(contain(23, position), stop("Covariance and variance cannot be specified at the same time!"), VPH <- list(new("NullSimVector")))				
		} else {
			PH <- list(new("NullSymMatrix"))
			ifelse(contain(22, position), RPH <- List[position == 22], RPH <- list(new("NullSymMatrix")))
			ifelse(contain(23, position), VPH <- List[position == 23], VPH <- list(new("NullSimVector")))
		}
		ifelse(contain(24, position), KA <- List[position == 24], KA <- list(new("NullSimVector")))
		if(contain(28, position)) {
			ifelse(contain(25, position), stop("TH and RTH cannot be specified at the same time!"), RTH <- list(new("NullSimMatrix")))
			TH <- List[position == 28]
		} else {
			ifelse(contain(25, position), RTH <- List[position == 25], RTH <- list(new("NullSimMatrix")))
		}
		Output <- new("SimMisspec", LY=LY[[1]], TE=TE[[1]], RTE=RTE[[1]], VTE=VTE[[1]], VY=VY[[1]], MY=MY[[1]], TY=TY[[1]], BE=BE[[1]], PS=PS[[1]], RPS=RPS[[1]], VPS=VPS[[1]], VE=VE[[1]], AL=AL[[1]], ME=ME[[1]],
					LX=LX[[1]], TD=TD[[1]], RTD=RTD[[1]], VTD=VTD[[1]], VX=VX[[1]], MX=MX[[1]], TX=TX[[1]], GA=GA[[1]], PH=PH[[1]], RPH=RPH[[1]], VPH=VPH[[1]], KA=KA[[1]], TH=TH[[1]], RTH=RTH[[1]], modelType="SEM.exo")	
	} else {
		Output <- new("SimMisspec", LY=LY[[1]], TE=TE[[1]], RTE=RTE[[1]], VTE=VTE[[1]], VY=VY[[1]], MY=MY[[1]], TY=TY[[1]], BE=BE[[1]], PS=PS[[1]], RPS=RPS[[1]], VPS=VPS[[1]], VE=VE[[1]], AL=AL[[1]], ME=ME[[1]], modelType="SEM")	
	}
	return(Output)
}
