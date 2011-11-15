# plotPower
# Methods -- simsem package
# This function will plot sampling distributions of fit indices that visualize power
# Generic Function: plotPower(object.alt, object.null, ...)
# Argument:
#	object.alt: 	The object that saves fit indices for alternative hypothesis
#	object.null:	The object of null hypothesis. It can be vector of cutoffs or raw data of fit indices of null hypothesis.
#	...:			Other arguments such as which fit indices will be used.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 9, 2011

setMethod("plotPower", signature(object.alt="data.frame", object.null="vector"), definition=function(object.alt, object.null, used.fit=NULL) {
	plotCutoff(object.alt, object.null, used.fit=used.fit)
})
#Arguments: 
#	object.alt:		data.frame.c of alternative hypothesis that users wish to plot their sampling distribution
# 	object.null:	A priori cutoffs for fit indices based on null hypothesis, saved in a vector
#	used.fit:		The name of fit indices that researchers wish to plot
#Description: 	This function will plot alternative sampling distributions with their cutoffs.
#Return: 		NONE. Just plot.
	
setMethod("plotPower", signature(object.alt="SimResult", object.null="vector"), definition=function(object.alt, object.null, used.fit=NULL) {
	plotCutoff(object.alt@Fit, object.null, used.fit=used.fit)
})
#Arguments: 
#	object.alt:		SimResult.c of alternative hypothesis that users wish to plot their sampling distribution
# 	object.null:	A priori cutoffs for fit indices based on null hypothesis, saved in a vector
#	used.fit:		The name of fit indices that researchers wish to plot
#Description: 	This function will extract fit indices information and plot alternative sampling distributions with their cutoffs.
#Return: 		NONE. Just plot.
	
setMethod("plotPower", signature(object.alt="data.frame", object.null="data.frame"), definition=function(object.alt, object.null, alpha, used.fit=NULL) {
	percentile <- 1 - alpha
	cutoff <- getCutoff(object.null, alpha)
	if(is.null(used.fit)) used.fit <- c("Chi", "AIC", "BIC", "RMSEA", "CFI", "TLI", "SRMR")
	object.alt <- as.data.frame(object.alt[,used.fit])
	object.null <- as.data.frame(object.null[,used.fit])
	colnames(object.alt) <- colnames(object.null) <- used.fit
	no.NA.object.alt <- !apply(object.alt, 2, is.na.vector)
	no.NA.object.null <- !apply(object.null, 2, is.na.vector)
	temp.name.alt <- colnames(object.alt)[no.NA.object.alt]
	temp.name.null <- colnames(object.null)[no.NA.object.null]
	object.alt <- as.data.frame(object.alt[,no.NA.object.alt])
	object.null <- as.data.frame(object.null[,no.NA.object.null])
	colnames(object.alt) <- temp.name.alt
	colnames(object.null) <- temp.name.null
	common.name <- intersect(colnames(object.alt), colnames(object.null))
	object.alt <- as.data.frame(object.alt[,common.name])
	object.null <- as.data.frame(object.null[,common.name])
	colnames(object.alt) <- colnames(object.null) <- common.name
	cutoff <- cutoff[common.name]
	if(length(common.name) == 2) {
		obj <- par(mfrow = c(1, 2))
	} else if(length(common.name) == 3) {
		obj <- par(mfrow = c(1, 3))
	} else if(length(common.name) > 3) {
		obj <- par(mfrow = c(2, ceiling(length(common.name)/2)))
	} else if(length(common.name) == 1) {
		# Intentionally leaving as blank
	} else {
		stop("Some errors occur")
	}
	for(i in 1:length(common.name)) {
		swap <- sum(common.name[i] == c("CFI", "TLI")) > 0
		overlap.hist(object.null[,i], object.alt[,i], main=common.name[i], xlab="Value", colors=c("yellow", "skyblue", "lightgreen"),
			swap=swap)
		cutoff1 <- quantile(object.null[,i], percentile, na.rm = TRUE)
		abline(v = cutoff[i], lty=1, lwd=3)
		position <- "topright"
		if(swap) position <- "topleft"
		legend(position, c("Null","Alternative"), cex=1, bty="n", fill=c("yellow", "skyblue"))
	}
	if(length(common.name) > 1) par(obj)
})
#Arguments: 
#	object.alt:		data.frame.c of alternative hypothesis that users wish to plot their sampling distribution
# 	object.null:	data.frame.c of null hypothesis that users wish to plot their sampling distribution
#	alpha:			A priori alpha that users wish to find fit indices cutoffs.
#	used.fit:		The name of fit indices that researchers wish to plot
#Description: 	This function will plot overlapping sampling distributions with cutoffs.
#Return: 		NONE. Just plot.
	
setMethod("plotPower", signature(object.alt="SimResult", object.null="SimResult"), definition=function(object.alt, object.null, alpha, used.fit=NULL) {
	plotPower(object.alt@Fit, object.null@Fit, alpha, used.fit)
})
#Arguments: 
#	object.alt:		SimResult.c of alternative hypothesis that users wish to plot their sampling distribution
# 	object.null:	SimResult.c of null hypothesis that users wish to plot their sampling distribution
#	alpha:			A priori alpha that users wish to find fit indices cutoffs.
#	used.fit:		The name of fit indices that researchers wish to plot
#Description: 	This function will extract data.frame of fit indices values from SimResult.c and pass it to the same function for data.frame.c.
#Return: 		NONE. Just plot.
