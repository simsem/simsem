# create.free.parameters
# Function -- simsem package
# Create free parameters object from model specification
# Function: create.free.parameters(object)
# Argument:
#	object: 	SimSet.c that users wish to transform it to SimFreeParam.c
# Return: 	SimFreeParam.c that saves only free parameters and values of fixed parameters
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 6, 2011

create.free.parameters <- function(object) {
	if(!is(object, "SimSet")) stop("The attribute is not a SimSet object.")
	#The free parameters will be only used. This will not affect the program.
	if(is.null.object(object@AL) & !is.null.object(object@ME)) object@AL <- object@ME
	if(is.null.object(object@TY) & !is.null.object(object@MY)) object@TY <- object@MY
	if(is.null.object(object@TX) & !is.null.object(object@MX)) object@TX <- object@MX
	if(is.null.object(object@VTE) & !is.null.object(object@VY)) object@VTE <- object@VY
	if(is.null.object(object@VTD) & !is.null.object(object@VX)) object@VTD <- object@VX
	if(is.null.object(object@VPS) & !is.null.object(object@VE)) object@VPS <- object@VE
	LY <- object@LY@free
	is.measurement.Y <- !(is.null.object(LY))
	TE <- object@TE@free
	if(is.measurement.Y) {
		VTE <- object@VTE@free
		ifelse(is.null.object(VTE), diag(TE) <- NA, diag(TE) <- VTE)
	}
	TY <- object@TY@free
	if(is.measurement.Y & is.default(object@TY)) TY <- rep(NA, nrow(LY))
	BE <- object@BE@free
	PS <- object@PS@free 
	VPS <- object@VPS@free
	ifelse(is.null.object(VPS), ifelse(is.measurement.Y, diag(PS) <- 1, diag(PS) <- NA), diag(PS) <- VPS)
	AL <- object@AL@free
	if(is.default(object@AL)) {
		ifelse(is.measurement.Y, AL <- rep(0, ncol(PS)), AL <- rep(NA, ncol(PS)))
	}
	#-- Exogeneous Variable --
	LX <- object@LX@free
	TD <- object@TD@free 
	GA <- object@GA@free
	PH <- object@PH@free	
	KA <- object@KA@free
	TX <- object@TX@free
	TH <- object@TH@free
	if(!is.null.object(PH)) {
		VPH <- object@VPH@free
		is.measurement.X <- !is.null.object(LX)
		ifelse(is.null.object(VPH) | (sum(VPH != 1) == 0), ifelse(is.measurement.X, diag(PH) <- 1, diag(PH) <- NA), diag(PH) <- VPH)
		if(is.default(object@KA)) ifelse(is.measurement.X, KA <- rep(0, ncol(PH)), KA <- rep(NA, ncol(PH)))
		if(is.measurement.X) {
			VTD <- object@VTD@free
			ifelse(is.null.object(VTD), diag(TD) <- NA, diag(TD) <- VTD)
			if(is.default(object@TX)) TX <- rep(NA, nrow(LX))
		}
	}
	Output <- new("SimFreeParam", LY=LY, TE=TE, BE=BE, PS=PS, AL=AL, TY=TY,
			LX=LX, TD=TD, TX=TX, GA=GA, PH=PH, KA=KA, TH=TH, modelType=object@modelType)	
}

# Example
#	loading <- matrix(0, 6, 2)
#	loading[1:3, 1] <- NA
#	loading[4:6, 2] <- NA
#	loadingValues <- matrix(0, 6, 2)
#	loadingValues[1:3, 1] <- 0.7
#	loadingValues[4:6, 2] <- 0.7
#	LX <- simMatrix(loading, loadingValues)
#	latent.cor <- matrix(NA, 2, 2)
#	diag(latent.cor) <- 1
#	PH <- symMatrix(latent.cor, 0.5)
#	error.cor <- matrix(0, 6, 6)
#	diag(error.cor) <- 1
#	TD <- symMatrix(error.cor)
#	indicator.mean <- rep(NA, 6)
#	MX <- simVector(indicator.mean, 0)
#	CFA.Model <- simSetCFA(LX = LX, PH = PH, TD = TD, MX = MX)
#	free <- create.free.parameters(CFA.Model)

