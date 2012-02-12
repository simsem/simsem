# summaryParam
# Methods -- simsem package
# This function will summarize the obtained parameter estimates and standard error.
# Generic Function: summaryParam(object, ...)
# Argument:
#	object: 	The object that users wish to find summary of the parameters.
#	...:			Other arguments (None is identified now)
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: November 15, 2011

setMethod("summaryParam", signature(object="SimResult"), definition=function(object, alpha=0.05, detail=FALSE) {
	coef <- colMeans(object@coef, na.rm=TRUE)
	real.se <- sapply(object@coef, sd, na.rm=TRUE)
	estimated.se <- colMeans(object@se, na.rm=TRUE)
	estimated.se[estimated.se==0] <- NA
	z <- object@coef/object@se
	crit.value <- qnorm(1 - alpha/2)
	sig <- abs(z) > crit.value
	pow <- apply(sig, 2, mean, na.rm=TRUE)
	result <- cbind(coef, real.se, estimated.se, pow)
	colnames(result) <- c("Estimate Average", "Estimate SD", "Average SE", "Power (Not equal 0)")
	if(!is.null.object(object@paramValue)) {
		nRep <- nrow(object@coef)
		nParam <- ncol(object@coef)
		paramValue <- object@paramValue	
		if(nrow(object@paramValue) == 1) paramValue <- matrix(unlist(rep(paramValue, nRep)), nRep, nParam, byrow=T)
		biasParam <- object@coef - paramValue
		crit <- qnorm(1 - alpha/2)
		lowerBound <- object@coef - crit * object@se
		upperBound <- object@coef + crit * object@se
		cover <- (paramValue > lowerBound) & (paramValue < upperBound)
		
		average.param <- apply(paramValue, 2, mean, na.rm=TRUE)
		sd.param <- apply(paramValue, 2, sd, na.rm=TRUE)
		average.bias <- apply(biasParam, 2, mean, na.rm=TRUE)
		perc.cover <- apply(cover, 2, mean, na.rm=TRUE)
		sd.bias <- apply(biasParam, 2, sd, na.rm=TRUE)
		perc.cover[estimated.se == 0] <- NA
		
		# Estimate = (Estimate - Obtain Param) + (Obtain Param - Exp Param) + Exp Param 
		# Var(Estimate) = Var(Estimate - Obtain Param) + Var(Obtain Param - Exp Param) + Cov(Estimate - Obtain Param, Obtain Param - Exp Param)
		# Var(Estimate) = Var(Bias) + Var(Obtain Param) + Cov(Bias, Obtain Param)
		# Estimated SE = sqrt(Var(Bias))
		result2 <- cbind(average.param, sd.param, average.bias, sd.bias, perc.cover)
		colnames(result2) <- c("Average Param", "SD Param", "Average Bias", "SD Bias","Coverage")
		if(nrow(object@paramValue) == 1) result2 <- result2[,c(1, 3, 5)]
		result <- data.frame(result, result2)
		if(detail){
			relative.bias <- biasParam/paramValue
			relBias <- apply(relative.bias, 2, mean, na.rm=TRUE)
			relBias[is.nan(relBias)] <- NA
			std.bias <- NULL
			relative.bias.se <- NULL
			if(nrow(object@paramValue) == 1) {
				std.bias <- average.bias/real.se
				relative.bias.se <- (estimated.se - real.se)/real.se
			} else {
				std.bias <- average.bias/sd.bias
				relative.bias.se <- (estimated.se - sd.bias)/sd.bias
			}
			result3 <- cbind(relBias, std.bias, relative.bias.se)
			colnames(result3) <- c("Rel Bias", "Std Bias", "Rel SE Bias")
			result <- data.frame(result, result3)
		}
	}
	if(!is.null.object(object@FMI1) & !is.null.object(object@FMI2)) {
		nRep <- nrow(object@coef)
		nFMI1 <- ncol(object@FMI1)
		FMI1 <- object@FMI1	
		FMI2 <- object@FMI2	
		average.FMI1 <- apply(FMI1, 2, mean, na.rm=TRUE)
		sd.FMI1 <- apply(FMI1, 2, sd, na.rm=TRUE)
		average.FMI2 <- apply(FMI2, 2, mean, na.rm=TRUE)
		sd.FMI2 <- apply(FMI2, 2, sd, na.rm=TRUE)

		resultFMI <- cbind(average.FMI1, sd.FMI1, average.FMI2, sd.FMI2)
		colnames(resultFMI) <- c("Average FMI1", "SD FMI1", "Average FMI2", "SD FMI2")
		result <- data.frame(result, resultFMI)
	}
	return(as.data.frame(result))
})
#Arguments: 
#	object:		SimResult.c of alternative hypothesis that users wish to find summary of parameters and standard errors
#	alpha:		A priori alpha level
#Description: 	This function will find mean of estimates, sd of estimates, mean of standard errors, and power of rejection (null hypothesis of population = 0) from a priori alpha level.
#Return: 		data.frame.c that contains those information of each parameter.

setMethod("summaryParam", signature(object="SimModelOut"), definition=function(object, alpha=0.05) {
	lab <- make.labels(object@param, "OpenMx")
	coef <- vectorize.object(object@coef, lab)
	se <- vectorize.object(object@se, lab)
	se[se==0] <- NA
	z <- coef/se
	p <- (1 - pnorm(abs(z))) * 2
	result <- cbind(coef, se, z, p)
	colnames(result) <- c("Estimate", "SE", "z", "p")
	if(!is.null.object(object@paramValue)) {
		paramValue <- vectorize.object(object@paramValue, lab)		
		biasParam <- vectorize.object(subtractObject(object@coef, object@paramValue), lab)
		crit <- qnorm(1 - alpha/2)
		lowerBound <- coef - crit * se
		upperBound <- coef + crit * se
		cover <- (paramValue > lowerBound) & (paramValue < upperBound)
		result <- data.frame(result, Param=paramValue, Bias=biasParam, Coverage=cover)
	}
	return(as.data.frame(result))
})
#Arguments: 
#	object:		SimModelOut.c of alternative hypothesis that users wish to find summary of parameters and standard errors
#Description: 	This function will find estimates, standard error, Wald statistic, and p value (null hypothesis of population = 0).
#Return: 		data.frame.c that contains those information of each parameter.

setMethod("summaryParam", signature(object="SimModelMIOut"), definition=function(object, alpha=0.05) {
	lab <- make.labels(object@param, "OpenMx")
	coef <- vectorize.object(object@coef, lab)
	se <- vectorize.object(object@se, lab)
	FMI1 <- vectorize.object(object@FMI1, lab)
	FMI2 <- vectorize.object(object@FMI2, lab)
	se[se==0] <- NA
	z <- coef/se
	p <- (1 - pnorm(abs(z))) * 2
	result <- cbind(coef, se, z, p, FMI1, FMI2)
	colnames(result) <- c("Estimate", "SE", "z", "p", "FMI1", "FMI2")
	if(!is.null.object(object@paramValue)) {
		paramValue <- vectorize.object(object@paramValue, lab)		
		biasParam <- vectorize.object(subtractObject(object@coef, object@paramValue), lab)
		crit <- qnorm(1 - alpha/2)
		lowerBound <- coef - crit * se
		upperBound <- coef + crit * se
		cover <- (paramValue > lowerBound) & (paramValue < upperBound)
		result <- data.frame(result, Param=paramValue, Bias=biasParam, Coverage=cover)
	}
	return(as.data.frame(result))
})
#Arguments: 
#	object:		SimModelOut.c of alternative hypothesis that users wish to find summary of parameters and standard errors
#Description: 	This function will find estimates, standard error, Wald statistic, and p value (null hypothesis of population = 0).
#Return: 		data.frame.c that contains those information of each parameter.
