# pValue
# Methods -- simsem package
# Find a p-value from an object
# Argument:
#	target: 	Values used to find p values
# Return: p values
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)

setMethod("pValue", signature(target="numeric", dist="vector"), definition=function(target, dist, revDirec=FALSE){
	if(revDirec) {
		return(mean(target<=dist, na.rm = TRUE))
	} else {
		return(mean(target>=dist, na.rm = TRUE))
	}
}
)
#Arguments: 
#	target:		target number to find p value
# 	dist:	a vector of data
#	revDirec:	If TRUE, find the p value from the right hand side. If FALSE, find the p value from the left hand side.
#Description: 	Find a p value of a number from a vector
#Return: 		NONE. Just plot.
# Example
#	pValue(0.5, rnorm(1000, 0, 1))

setMethod("pValue", signature(target="numeric", dist="data.frame"), definition=function(target, dist, revDirec=NULL, asLogical=FALSE){
	if(length(target) != ncol(dist)) stop("The length of target and the number of columns of dist are not equal")
	numVar <- length(target)
	if(is.null(revDirec)) {
		revDirec <- rep(FALSE, numVar)
	} else {
		if(length(revDirec) != numVar) stop("The length of revDirec and the number of columns of dist are not equal")
	}
	if(asLogical) {
		result <- NULL
		for(i in 1:numVar) {
			if(revDirec[i]) {
				result <- cbind(result, target[i] <= dist[,i])
			} else {
				result <- cbind(result, target[i] >= dist[,i])
			}
		}
		return(result)
	} else {
		result <- rep(NA, numVar)
		for(i in 1:numVar) {
			result[i] <- pValue(target[i], dist[,i], revDirec[i])
		}
		return(result)
	}
}
)
#Arguments: 
#	target:		target vector to find p values
# 	dist:		a data frame of data
#	revDirec:	A vector of logicals. If TRUE, find the p value from the right hand side. If FALSE, find the p value from the left hand side.
#	asLogical:	If TRUE, to return a logical of value that the average equal to p value
#Description: 	Find a p value of a number from a vector
#Return: 		A p value or a vector (or matrix) of logical values
# Example
#	pValue(c(0.5, 0.2), data.frame(rnorm(1000, 0, 1), runif(1000, 0, 1)))

setMethod("pValue", signature(target="SimModelOut", dist="SimResult"), definition=function(target, dist, usedFit=NULL){
	dist <- clean(dist)
	if(is.null(usedFit)) usedFit <- getKeywords()$usedFit
	revDirec <- !(usedFit %in% c("CFI", "TLI"))
	logicalMat <- pValue(target@fit[usedFit], dist@fit[,usedFit], revDirec, asLogical=TRUE)
	result <- apply(logicalMat, 2, mean, na.rm=TRUE)
	names(result) <- usedFit
	andRule <- mean(apply(logicalMat, 1, all), na.rm=TRUE)
	orRule <- mean(apply(logicalMat, 1, any), na.rm=TRUE)
	c(result, andRule=andRule, orRule=orRule)
}
)
#Arguments: 
#	target:		target SimModelOut class. The fit indices will be used to find p values (Monte Carlo approach)
# 	dist:		SimResult class to compare with
#	usedFit:	Selected fit indices used to find a p value
#Description: 	Find a p value of a SimModelOut object from a SimResult object
#Return: 		p value based on each fit index and the combination rule
#Example
#library(lavaan)
#loading <- matrix(0, 9, 3)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loading[7:9, 3] <- NA
#model <- simParamCFA(LY=loading)
#SimModel <- simModel(model, indLab=paste("x", 1:9, sep=""))
#u2 <- simUnif(-0.2, 0.2)
#loading.trivial <- matrix(NA, 9, 3)
#loading.trivial[is.na(loading)] <- 0
#LY.trivial <- simMatrix(loading.trivial, "u2")
#mis <- simMisspecCFA(LY = LY.trivial)
#out <- run(SimModel, HolzingerSwineford1939)
#Output2 <- runFit(out, HolzingerSwineford1939, 20, mis)
#pValue(out, Output2)
