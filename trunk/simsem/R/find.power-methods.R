# getPower
# Methods -- simsem package
# This function will find a power of each fit index based on specified cutoffs of each fit index
# Generic Function: getPower(object.alt, cutoff, reverse = FALSE, used.fit=NULL)
# Argument:
#	object.alt: 	The object (SimResult.c, data.frame.c, or matrix.c) that contains values of fit indices in each replication.
#	cutoff:		Cutoff for each fit index saved in a vector.
#	reverse:	The default is to getPower on the side that indicates worse fit (the right side of RMSEA or the left side of CFI). If specifying as TRUE, the directions are reversed.
#	used.fit:	The name of fit indices that researchers wish to getPower.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 9, 2011

setMethod("getPower", signature(object.alt="data.frame"), definition=function(object.alt, cutoff, reverse=FALSE, used.fit=NULL) {
	if(is.null(used.fit)) used.fit <- c("Chi", "AIC", "BIC", "RMSEA", "CFI", "TLI", "SRMR")
	if(is.null(names(cutoff)) && length(cutoff) == 7) names(cutoff) <- used.fit
	common.name <- Reduce(intersect, list(colnames(object.alt), names(cutoff), used.fit))
	temp <- rep(NA, length(common.name))
	names(temp) <- common.name
	object.alt <- as.data.frame(object.alt[,common.name])
	cutoff <- cutoff[common.name]
	for(i in 1:length(common.name)) {
		temp[i] <- pvalue(object.alt[,i], cutoff[i], reverse)
	}
	if(contain("TLI", common.name)) temp["TLI"] <- 1 - temp["TLI"]
	if(contain("CFI", common.name)) temp["CFI"] <- 1 - temp["CFI"]
	return(temp)
})
#Arguments: 
#	object.alt:		data.frame.c that users wish getPower from
# 	cutoff:		As descibed in the beginning of the file.
#	reverse:	As descibed in the beginning of the file.
#	used.fit:	As descibed in the beginning of the file.
#Description: 	This function will getPower from data frame based on pvalue function.
#Return: 		vector.c of power of each fit index

setMethod("getPower", signature(object.alt="SimResult"), definition=function(object.alt, cutoff, reverse=FALSE, used.fit=NULL) {
	Result <- object.alt@Fit
	output <- getPower(Result, cutoff, reverse, used.fit)
	return(output)
})
#Arguments: 
#	object.alt:		SimResult.c that users wish getPower from
# 	cutoff:		As descibed in the beginning of the file.
#	reverse:	As descibed in the beginning of the file.
#	used.fit:	As descibed in the beginning of the file.
#Description: 	This function will extract fit index data frame and pass to this function for data frame.
#Return: 		vector.c of power of each fit index
#Examples:
#loading.null <- matrix(0, 6, 1)
#loading.null[1:6, 1] <- NA
#LX.NULL <- simMatrix(loading.null, 0.7)
#PH.NULL <- symMatrix(diag(1))
#TD <- symMatrix(diag(6))
#CFA.Model.NULL <- simSetCFA(LY = LX.NULL, PS = PH.NULL, TE = TD)
#SimData.NULL <- simData(500, CFA.Model.NULL)
#SimModel <- simModel(CFA.Model.NULL)
#Output.NULL <- simResult(SimData.NULL, SimModel, 50)
#Cut.NULL <- getCutoff(Output.NULL, 0.95)
#u79 <- simUnif(0.7, 0.9)
#loading.alt <- matrix(0, 6, 2)
#loading.alt[1:3, 1] <- NA
#loading.alt[4:6, 2] <- NA
#LX.ALT <- simMatrix(loading.alt, 0.7)
#latent.cor.alt <- matrix(NA, 2, 2)
#diag(latent.cor.alt) <- 1
#PH.ALT <- symMatrix(latent.cor.alt, "u79")
#CFA.Model.ALT <- simSetCFA(LY = LX.ALT, PS = PH.ALT, TE = TD)
#SimData.ALT <- simData(500, CFA.Model.ALT)
#Output.ALT <- simResult(SimData.ALT, SimModel, 50)
#getPower(Output.ALT, Cut.NULL)
#Rule.of.thumb <- c(RMSEA=0.05, CFI=0.95, TLI=0.95, SRMR=0.06)
#getPower(Output.ALT, Rule.of.thumb, used.fit=c("RMSEA", "CFI", "TLI", "SRMR"))

setMethod("getPower", signature(object.alt="matrix"), definition=function(object.alt, cutoff, reverse=FALSE, used.fit=NULL) {
	object <- as.data.frame(object.alt)
	output <- getPower(object, cutoff, reverse, used.fit)
	return(output)	
})
#Arguments: 
#	object.alt:		data.frame.c that users wish getPower from
# 	cutoff:		As descibed in the beginning of the file.
#	reverse:	As descibed in the beginning of the file.
#	used.fit:	As descibed in the beginning of the file.
#Description: 	This function will change matrix.c to data.frame.c and pass to this function with data.frame.c
#Return: 		vector.c of power of each fit index

