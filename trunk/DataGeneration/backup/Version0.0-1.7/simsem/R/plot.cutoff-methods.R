# plot.cutoff
# Methods -- simsem package
# This function will plot sampling distributions of fit indices with vertical lines of cutoffs
# Generic Function: plot.cutoff(object, ...)
# Argument:
#	object: 	The object (simResult.c or data.frame.c) that contains values of fit indices in each distribution.
#	...:		Other argments such as cutoff values
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 9, 2011

setMethod("plot.cutoff", signature(object="data.frame"), definition=function(object, cutoff=NULL, reverse = FALSE, used.fit=NULL) {
	if(is.null(used.fit)) used.fit <- c("Chi", "AIC", "BIC", "RMSEA", "CFI", "TLI", "SRMR")
	object <- as.data.frame(object[,used.fit])
	cutoff <- cutoff[used.fit]
	object <- as.data.frame(object[,!apply(object, 2, is.na.vector)])
	colnames(object) <- used.fit
	if(ncol(object) == 2) {
		obj <- par(mfrow = c(1, 2))
	} else if(ncol(object) == 3) {
		obj <- par(mfrow = c(1, 3))
	} else if(ncol(object) > 3) {
		obj <- par(mfrow = c(2, ceiling(ncol(object)/2)))
	} else if(ncol(object) == 1) {
		# Intentionally leaving as blank
	} else {
		stop("Some errors occur")
	}
	for(i in 1:ncol(object)) {
		hist(object[,i], main = colnames(object)[i], breaks = 10, col="yellow", xlab = "Value")
		if(!is.null(cutoff)) abline(v = cutoff[i], col="red", lwd=3)
	}
	if(ncol(object) > 1) par(obj)
})
#Arguments: 
#	object:		data.frame.c that users wish to plot their sampling distribution
# 	cutoff:		A priori cutoffs for fit indices, saved in a vector
#	reverse:	The default is to find critical point on the side that indicates worse fit (the right side of RMSEA or the left side of CFI). If specifying as TRUE, the directions are reversed.
#	used.fit:	The name of fit indices that researchers wish to plot
#Description: 	This function plot sampling distributions and make vertical line as cutoffs
#Return: 		NONE. Just plot.

setMethod("plot.cutoff", signature(object="simResult"), definition=function(object, alpha=NULL, reverse = FALSE, used.fit=NULL) {
	cutoff <- NULL
	Data <- as.data.frame(object@Output)
	if(!is.null(alpha)) {
		if(reverse) alpha <- 1 - alpha
		cutoff <- find.cutoff(Data, alpha)
	}
	plot.cutoff(Data, cutoff, reverse, used.fit)
})
#Arguments: 
#	object:		data.frame.c that users wish to plot their sampling distributions
# 	alpha:		A priori alpha level to find cutoffs of fit indices
#	reverse:	The default is to find critical point on the side that indicates worse fit (the right side of RMSEA or the left side of CFI). If specifying as TRUE, the directions are reversed.
#	used.fit:	The name of fit indices that researchers wish to plot
#Description: 	This function will extract raw fit indices data and cutoff and pass it to the same function using data frame and vector of cutoffs
#Return: 		NONE. Just plot.
