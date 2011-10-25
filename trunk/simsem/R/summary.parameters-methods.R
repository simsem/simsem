# summary.parameters
# Methods -- simsem package
# This function will summarize the obtained parameter estimates and standard error.
# Generic Function: summary.parameters(object, ...)
# Argument:
#	object: 	The object that users wish to find summary of the parameters.
#	...:			Other arguments (None is identified now)
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 11, 2011

setMethod("summary.parameters", signature(object="simResult"), definition=function(object, alpha=0.05) {
	Estimates <- mean(object@Estimates, na.rm=TRUE)
	real.SE <- sd(object@Estimates, na.rm=TRUE)
	estimated.SE <- mean(object@SE, na.rm=TRUE)
	z <- object@Estimates/object@SE
	crit.value <- qnorm(1 - alpha/2)
	sig <- abs(z) > crit.value
	pow <- apply(sig, 2, mean, na.rm=TRUE)
	result <- cbind(Estimates, real.SE, estimated.SE, pow)
	colnames(result) <- c("Estimate Average", "Estimate SD", "Average SE", "Power (!= 0)")
	return(as.data.frame(result))
})
#Arguments: 
#	object:		data.frame.c of alternative hypothesis that users wish to plot their sampling distribution
#	alpha:		A priori alpha level
#Description: 	This function will find mean of estimates, sd of estimates, mean of standard errors, and power of rejection (null hypothesis of population = 0) from a priori alpha level.
#Return: 		data.frame.c that contains those information of each parameter.
