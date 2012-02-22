# createData
# function -- simsem package
# Create data from parameters
# Argument:
#	param: parameters used for creating data
#	sequential: TRUE if users wish to create data steps by steps from latent variables
#				FALSE if users wish to create data from means and covariances matrix of indicators only
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: February 21, 2012

createData <- function(paramSet, n, object, dataOnly, sequential=FALSE) {
	library(MASS)
	Data <- NULL
	modelType <- object@modelType
	param <- paramSet[["real"]]
	misspec <- paramSet[["misspec"]]
	usedParam <- NULL
	if(!is.null.object(object@misspec)) {		
		usedParam <- misspec
	} else {
		usedParam <- param
	}
	if(sequential) {
		# Have not implement yet
	} else {
		suff <- create.implied.MACS(usedParam)
		Data <- mvrnorm(n, suff$M, suff$CM)
	}
	varnames <- NULL
	if(modelType == "Path.exo") {
		nx <- ncol(run(object@param@PH))
		for(i in 1:nx) {
			temp <- paste("x", i, sep="")
			varnames <- c(varnames, temp)
		}
		for(i in 1:(ncol(Data) - nx)) {
			temp <- paste("y", i, sep="")
			varnames <- c(varnames, temp)
		}
	} else if(modelType == "SEM.exo") {
		nx <- nrow(run(object@param@LX))
		for(i in 1:nx) {
			temp <- paste("x", i, sep="")
			varnames <- c(varnames, temp)
		}	
		for(i in 1:(ncol(Data) - nx)) {
			temp <- paste("y", i, sep="")
			varnames <- c(varnames, temp)
		}
	} else {
		for(i in 1:ncol(Data)) {
			temp <- paste("y", i, sep="")
			varnames <- c(varnames, temp)
		}
	}
	colnames(Data) <- varnames
	Data <- as.data.frame(Data)
	if(dataOnly) {
		return(Data)
	} else {
		if(is.null(misspec)) misspec <- new("NullRSet")
		out <- new("SimDataOut", modelType=object@modelType, data=Data, param=create.free.parameters(object@param), paramOut=param, misspecOut=misspec, equalCon=object@equalCon)
		return(out)
	}
}
