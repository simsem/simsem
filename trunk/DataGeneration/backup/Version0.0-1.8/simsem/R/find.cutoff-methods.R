# find.cutoff
# Methods -- simsem package
# This function will find a cutoff of each fit index based on a priori alpha level from sampling distributions of fit indices
# Generic Function: find.cutoff(object, alpha, reverse=FALSE, used.fit=NULL)
# Argument:
#	object: 	The object (simResult.c, data.frame.c, or matrix.c) that contains values of fit indices in each distribution.
#	alpha:		a priori alpha level in finding cutoffs.
#	reverse:	The default is to find critical point on the side that indicates worse fit (the right side of RMSEA or the left side of CFI). If specifying as TRUE, the directions are reversed.
#	used.fit:	The name of fit indices that researchers wish to find cutoffs.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 9, 2011

setMethod("find.cutoff", signature(object="data.frame"), definition=function(object, alpha, reverse=FALSE, used.fit=NULL) {
	if(is.null(used.fit)) used.fit <- c("Chi", "AIC", "BIC", "RMSEA", "CFI", "TLI", "SRMR")
	percentile <- 1 - alpha
	if(reverse) percentile <- 1 - percentile
	object <- as.data.frame(object[,used.fit])
	temp <- rep(NA, ncol(object))
	temp <- apply(object, 2, quantile, probs = percentile, na.rm = TRUE)
	if(contain("TLI", colnames(object))) temp["TLI"] <- quantile(object[,"TLI"], 1 - percentile, na.rm = TRUE)
	if(contain("CFI", colnames(object))) temp["CFI"] <- quantile(object[,"CFI"], 1 - percentile, na.rm = TRUE)
	return(temp)
})
#Arguments: 
#	object:		data.frame.c that users wish find cutoffs from
# 	alpha:		As descibed in the beginning of the file.
#	reverse:	As descibed in the beginning of the file.
#	used.fit:	As descibed in the beginning of the file.
#Description: 	This function will find cutoff from data frame based on quantile function.
#Return: 		vector.c of cutoffs of each fit index

setMethod("find.cutoff", signature(object="simResult"), definition=function(object, alpha, reverse=FALSE, used.fit=NULL) {
	Result <- object@Fit
	output <- find.cutoff(Result, alpha, reverse, used.fit)
	return(output)
})
#Arguments: 
#	object:		simResult.c that users wish find cutoffs from
# 	alpha:		As descibed in the beginning of the file.
#	reverse:	As descibed in the beginning of the file.
#	used.fit:	As descibed in the beginning of the file.
#Description: 	This function will extract fit.indices from simResult object and pass to this function with data.frame.
#Return: 		vector.c of cutoffs of each fit index
#Examples:
#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#loadingValues[1:3, 1] <- 0.7
#loadingValues[4:6, 2] <- 0.7
#LX <- matrix.object(loading, loadingValues)
#latent.cor <- matrix(NA, 2, 2)
#diag(latent.cor) <- 1
#PH <- sym.matrix.object(latent.cor, 0.5)
#error.cor <- matrix(0, 6, 6)
#diag(error.cor) <- 1
#TD <- sym.matrix.object(error.cor)
#CFA.Model <- matrix.CFA.object(LY = LX, PS = PH, TE = TD)
#SimData <- data.object(200, CFA.Model)
#SimModel <- model.object(CFA.Model)
#Output <- result.object(SimData, SimModel, 500)
#find.cutoff(Output, 0.05)

setMethod("find.cutoff", signature(object="matrix"), definition=function(object, alpha, reverse=FALSE, used.fit=NULL) {
	object <- as.data.frame(object)
	output <- find.cutoff(object, alpha, reverse, used.fit)
	return(output)	
})
#Arguments: 
#	object:		matrix.c that users wish find cutoffs from
# 	alpha:		As descibed in the beginning of the file.
#	reverse:	As descibed in the beginning of the file.
#	used.fit:	As descibed in the beginning of the file.
#Description: 	This function will change matrix.c to data.frame.c and pass to this function with data.frame.
#Return: 		vector.c of cutoffs of each fit index

