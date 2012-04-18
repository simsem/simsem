# createFreeParameters
# Function -- simsem package
# Create free parameters object (SimParam class) from model specification (SimSet class)
# Function: createFreeParameters(object)
# Argument:
#	object: SimSet class that users wish to transform it to SimParam.c
# Return: 	SimParam class that saves only free parameters and values of fixed parameters
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)

createFreeParameters <- function(object) {
	if(!is(object, "SimSet")) stop("The attribute is not a SimSet object.")
	#The free parameters will be only used. This will not affect the program.
	if(isNullObject(object@AL) & !isNullObject(object@ME)) object@AL <- object@ME
	if(isNullObject(object@TY) & !isNullObject(object@MY)) object@TY <- object@MY
	if(isNullObject(object@TX) & !isNullObject(object@MX)) object@TX <- object@MX
	if(isNullObject(object@VTE) & !isNullObject(object@VY)) object@VTE <- object@VY
	if(isNullObject(object@VTD) & !isNullObject(object@VX)) object@VTD <- object@VX
	if(isNullObject(object@VPS) & !isNullObject(object@VE)) object@VPS <- object@VE
	LY <- object@LY@free
	is.measurement.Y <- !(isNullObject(LY))
	TE <- NULL
	if(isNullObject(object@TE)) {
		TE <- object@RTE@free
		if(is.measurement.Y) {
			VTE <- object@VTE@free
			ifelse(isNullObject(VTE), diag(TE) <- NA, diag(TE) <- VTE)
		}
	} else {
		TE <- object@TE@free
	}
	TY <- object@TY@free
	if(is.measurement.Y & isDefault(object@TY)) TY <- rep(NA, nrow(LY))
	BE <- object@BE@free
	PS <- NULL
	if(isNullObject(object@PS)) {
		PS <- object@RPS@free 
		VPS <- object@VPS@free
		ifelse(isNullObject(VPS), ifelse(is.measurement.Y, diag(PS) <- 1, diag(PS) <- NA), diag(PS) <- VPS)
	} else {
		PS <- object@PS@free
	}
	AL <- object@AL@free
	if(isDefault(object@AL)) {
		ifelse(is.measurement.Y, AL <- rep(0, ncol(PS)), AL <- rep(NA, ncol(PS)))
	}
	#-- Exogeneous Variable --
	LX <- object@LX@free
	TD <- object@RTD@free 
	if(!isNullObject(object@TD)) TD <- object@TD@free
	GA <- object@GA@free
	PH <- object@RPH@free	
	if(!isNullObject(object@PH)) PH <- object@PH@free
	KA <- object@KA@free
	TX <- object@TX@free
	TH <- object@RTH@free
	if(!isNullObject(object@TH)) TH <- object@TH@free
	if(!isNullObject(PH)) {
		is.measurement.X <- !isNullObject(LX)
		if(isNullObject(object@PH)) {
			VPH <- object@VPH@free
			ifelse(isNullObject(VPH) | (sum(VPH != 1) == 0), ifelse(is.measurement.X, diag(PH) <- 1, diag(PH) <- NA), diag(PH) <- VPH)
		}
		if(isDefault(object@KA)) ifelse(is.measurement.X, KA <- rep(0, ncol(PH)), KA <- rep(NA, ncol(PH)))
		if(is.measurement.X) {
			if(isNullObject(object@TD)) {
				VTD <- object@VTD@free
				ifelse(isNullObject(VTD), diag(TD) <- NA, diag(TD) <- VTD)
			}
			if(isDefault(object@TX)) TX <- rep(NA, nrow(LX))
		}
	}
	result <- NULL
	if(object@modelType == "CFA") {
		result <- simParamCFA(LY=LY, TY=TY, AL=AL, TE=TE, PS=PS)
	} else if (object@modelType == "Path") {
		result <- simParamPath(BE=BE, AL=AL, PS=PS)
	} else if (object@modelType == "Path.exo") {
		result <- simParamPath(BE=BE, AL=AL, PS=PS, GA=GA, KA=KA, PH=PH, exo=TRUE)
	} else if (object@modelType == "SEM") {
		result <- simParamSEM(LY=LY, TY=TY, BE=BE, AL=AL, TE=TE, PS=PS)	
	} else if (object@modelType == "SEM.exo") {
		result <- simParamSEM(LY=LY, TY=TY, BE=BE, AL=AL, TE=TE, PS=PS, LX=LX, TX=TX, GA=GA, KA=KA, TD=TD, PH=PH, TH=TH, exo=TRUE)	
	} else {
		stop("Something is wrong!")
	}
	return(result)	
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
#	RPH <- symMatrix(latent.cor, 0.5)
#	error.cor <- matrix(0, 6, 6)
#	diag(error.cor) <- 1
#	RTD <- symMatrix(error.cor)
#	indicator.mean <- rep(NA, 6)
#	MX <- simVector(indicator.mean, 0)
#	CFA.Model <- simSetCFA(LX = LX, RPH = RPH, RTD = RTD, MX = MX)
#	free <- createFreeParameters(CFA.Model)

