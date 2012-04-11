# getCutoff
# Methods -- simsem package
# This function will find a cutoff of each fit index based on a priori alpha level from sampling distributions of fit indices
# Generic Function: getCutoff(object, alpha, revDirec=FALSE, usedFit=NULL)
# Argument:
#	object: 	The object (SimResult.c, data.frame.c, or matrix.c) that contains values of fit indices in each distribution.
#	alpha:		a priori alpha level in finding cutoffs.
#	revDirec:	The default is to find critical point on the side that indicates worse fit (the right side of RMSEA or the left side of CFI). If specifying as TRUE, the directions are revDirecd.
#	usedFit:	The name of fit indices that researchers wish to getCutoffs.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 9, 2011

setMethod("getCutoff", signature(object="data.frame"), definition=function(object, alpha, revDirec=FALSE, usedFit=NULL) {
	if(is.null(usedFit)) usedFit <- c("Chi", "AIC", "BIC", "RMSEA", "CFI", "TLI", "SRMR")
	percentile <- 1 - alpha
	if(revDirec) percentile <- 1 - percentile
	object <- as.data.frame(object[,usedFit])
	temp <- rep(NA, ncol(object))
	temp <- apply(object, 2, quantile, probs = percentile, na.rm = TRUE)
	if(contain("TLI", colnames(object))) temp["TLI"] <- quantile(object[,"TLI"], 1 - percentile, na.rm = TRUE)
	if(contain("CFI", colnames(object))) temp["CFI"] <- quantile(object[,"CFI"], 1 - percentile, na.rm = TRUE)
	return(temp)
})
#Arguments: 
#	object:		data.frame.c that users wish getCutoffs from
# 	alpha:		As descibed in the beginning of the file.
#	revDirec:	As descibed in the beginning of the file.
#	usedFit:	As descibed in the beginning of the file.
#Description: 	This function will getCutoff from data frame based on quantile function.
#Return: 		vector.c of cutoffs of each fit index

setMethod("getCutoff", signature(object="SimResult"), definition=function(object, alpha, revDirec=FALSE, usedFit=NULL) {
	object <- clean(object)
	Result <- object@fit
	output <- getCutoff(Result, alpha, revDirec, usedFit)
	return(output)
})
#Arguments: 
#	object:		SimResult.c that users wish getCutoffs from
# 	alpha:		As descibed in the beginning of the file.
#	revDirec:	As descibed in the beginning of the file.
#	usedFit:	As descibed in the beginning of the file.
#Description: 	This function will extract fit.indices from SimResult object and pass to this function with data.frame.
#Return: 		vector.c of cutoffs of each fit index
#Examples:
#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#loadingValues[1:3, 1] <- 0.7
#loadingValues[4:6, 2] <- 0.7
#LX <- simMatrix(loading, loadingValues)
#latent.cor <- matrix(NA, 2, 2)
#diag(latent.cor) <- 1
#RPH <- symMatrix(latent.cor, 0.5)
#error.cor <- matrix(0, 6, 6)
#diag(error.cor) <- 1
#RTD <- symMatrix(error.cor)
#CFA.Model <- simSetCFA(LY = LX, RPS = RPH, RTE = RTD)
#SimData <- simData(CFA.Model, 200)
#SimModel <- simModel(CFA.Model)
#Output <- simResult(SimData, SimModel, 500)
#getCutoff(Output, 0.05)

setMethod("getCutoff", signature(object="matrix"), definition=function(object, alpha, revDirec=FALSE, usedFit=NULL) {
	object <- as.data.frame(object)
	output <- getCutoff(object, alpha, revDirec, usedFit)
	return(output)	
})
#Arguments: 
#	object:		matrix.c that users wish getCutoffs from
# 	alpha:		As descibed in the beginning of the file.
#	revDirec:	As descibed in the beginning of the file.
#	usedFit:	As descibed in the beginning of the file.
#Description: 	This function will change matrix.c to data.frame.c and pass to this function with data.frame.
#Return: 		vector.c of cutoffs of each fit index

