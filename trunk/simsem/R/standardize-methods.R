# standardize
# Methods -- simsem package
# Standardize the coefficients of an object
# Generic Function: standardize(object)
# Argument:
#	object: 	Desired object that users wish to standardize
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: February 21, 2012

setMethod("standardize", signature="SimModelOut", definition=function(object) {
		est <- object@coef
		result <- standardize(est)
		return(result)
	}
)
#Arguments: object is the SimModelOut class 
#Description: This function will extract the coefficients and standardize it
#Return: 	The SimRSet class that contains standardized values
#Example:
#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#loadingValues[1:3, 1] <- 0.7
#loadingValues[4:6, 2] <- 0.7
#LX <- simMatrix(loading, loadingValues)
#summary(LX)

#latent.cor <- matrix(NA, 2, 2)
#diag(latent.cor) <- 1
#PH <- symMatrix(latent.cor, 0.5)

#error.cor <- matrix(0, 6, 6)
#diag(error.cor) <- 1
#TD <- symMatrix(error.cor)

#CFA.Model <- simSetCFA(LX = LX, PH = PH, TD = TD)
#SimData <- simData(CFA.Model, 200)

#SimModel <- simModel(CFA.Model)
#standardize(run(SimModel, run(SimData)))

setMethod("standardize", signature="SimRSet", definition=function(object) {
		type <- object@modelType
		sdIndicator <- sqrt(diag(diag(createImpliedMACS(object)$CM)))
		sdLatent <- NULL
		if(object@modelType == "CFA") {
			sdLatent <- sqrt(diag(diag(object@PS)))
		} else {
			M <- object
			if(M@modelType == "SEM") M@modelType <- "Path"
			if(M@modelType == "SEM.exo") M@modelType <- "Path.exo"
			sdLatent <- sqrt(diag(diag(createImpliedMACS(M)$CM)))
		}
		result <- new("SimRSet", modelType=type)

		if(type == "CFA") {
			result@TY <- solve(sdIndicator) %*% object@TY
			result@LY <- solve(sdIndicator) %*% object@LY %*% sdLatent
			result@TE <- solve(sdIndicator) %*% object@TE %*% solve(sdIndicator)
			result@AL <- solve(sdLatent) %*% object@AL
			result@PS <- solve(sdLatent) %*% object@PS %*% solve(sdLatent)
		} else if (type == "Path") {
			result@AL <- solve(sdIndicator) %*% object@AL
			result@PS <- solve(sdIndicator) %*% object@PS %*% solve(sdIndicator)
			result@BE <- solve(sdIndicator) %*% object@BE %*% sdIndicator
		} else if (type == "Path.exo") {
			ny <- nrow(object@GA)
			nx <- ncol(object@GA)
			sdX <- sdIndicator[1:nx, 1:nx]
			sdY <- sdIndicator[(nx+1):(nx+ny), (nx+1):(nx+ny)]
			result@AL <- solve(sdY) %*% object@AL
			result@PS <- solve(sdY) %*% object@PS %*% solve(sdY)
			result@BE <- solve(sdY) %*% object@BE %*% sdY
			result@GA <- solve(sdY) %*% object@GA %*% sdX
			result@PH <- solve(sdX) %*% object@PH %*% solve(sdX)
			result@KA <- solve(sdX) %*% object@KA
		} else if (type == "SEM") {
			result@AL <- solve(sdLatent) %*% object@AL
			result@PS <- solve(sdLatent) %*% object@PS %*% solve(sdLatent)
			result@BE <- solve(sdLatent) %*% object@BE %*% sdLatent
			result@TY <- solve(sdIndicator) %*% object@TY
			result@LY <- solve(sdIndicator) %*% object@LY %*% sdLatent
			result@TE <- solve(sdIndicator) %*% object@TE %*% solve(sdIndicator)
		} else if (type == "SEM.exo") {
			nk <- ncol(object@GA)
			ne <- nrow(object@GA)
			nx <- nrow(object@LX)
			ny <- nrow(object@LY)
			sdX <- sdIndicator[1:nx, 1:nx]
			sdY <- sdIndicator[(nx+1):(nx+ny), (nx+1):(nx+ny)]
			sdK <- sdLatent[1:nk, 1:nk]
			sdE <- sdLatent[(nk+1):(nk+ne), (nk+1):(nk+ne)]
			result@AL <- solve(sdE) %*% object@AL
			result@PS <- solve(sdE) %*% object@PS %*% solve(sdE)
			result@BE <- solve(sdE) %*% object@BE %*% sdE
			result@TY <- solve(sdY) %*% object@TY
			result@LY <- solve(sdY) %*% object@LY %*% sdE
			result@TE <- solve(sdY) %*% object@TE %*% solve(sdY)
			result@GA <- solve(sdE) %*% object@GA %*% sdK
			result@PH <- solve(sdK) %*% object@PH %*% solve(sdK)
			result@KA <- solve(sdK) %*% object@KA
			result@TX <- solve(sdX) %*% object@TX
			result@LX <- solve(sdX) %*% object@LX %*% sdK
			result@TD <- solve(sdX) %*% object@TD %*% solve(sdX)
			result@TH <- solve(sdX) %*% object@TH %*% solve(sdY)
		} else {
			stop("The coefficient matrix does not have correct model type")
		}
		return(result)
	}
)
#Arguments: object is the SimRSet class 
#Description: This function will extract the coefficients and standardize it
#Return: 	The SimRSet class that contains standardized values
#Example:
#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#loadingValues[1:3, 1] <- 0.7
#loadingValues[4:6, 2] <- 0.7
#LX <- simMatrix(loading, loadingValues)
#summary(LX)

#latent.cor <- matrix(NA, 2, 2)
#diag(latent.cor) <- 1
#PH <- symMatrix(latent.cor, 0.5)

#error.cor <- matrix(0, 6, 6)
#diag(error.cor) <- 1
#TD <- symMatrix(error.cor)

#CFA.Model <- simSetCFA(LX = LX, PH = PH, TD = TD)
#set <- reduceMatrices(run(CFA.Model))
