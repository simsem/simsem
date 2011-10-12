# find.power
# Methods -- simsem package
# This function will find a power of each fit index based on specified cutoffs of each fit index
# Generic Function: find.power(object.alt, cutoff, reverse = FALSE, used.fit=NULL)
# Argument:
#	object.alt: 	The object (simResult.c, data.frame.c, or matrix.c) that contains values of fit indices in each replication.
#	cutoff:		Cutoff for each fit index saved in a vector.
#	reverse:	The default is to find power on the side that indicates worse fit (the right side of RMSEA or the left side of CFI). If specifying as TRUE, the directions are reversed.
#	used.fit:	The name of fit indices that researchers wish to find power.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 9, 2011

setMethod("find.power", signature(object.alt="data.frame"), definition=function(object.alt, cutoff, reverse=FALSE, used.fit=NULL) {
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
#	object.alt:		data.frame.c that users wish find power from
# 	cutoff:		As descibed in the beginning of the file.
#	reverse:	As descibed in the beginning of the file.
#	used.fit:	As descibed in the beginning of the file.
#Description: 	This function will find power from data frame based on pvalue function.
#Return: 		vector.c of power of each fit index

setMethod("find.power", signature(object.alt="simResult"), definition=function(object.alt, cutoff, reverse=FALSE, used.fit=NULL) {
	Result <- object.alt@Fit
	output <- find.power(Result, cutoff, reverse, used.fit)
	return(output)
})
#Arguments: 
#	object.alt:		simResult.c that users wish find power from
# 	cutoff:		As descibed in the beginning of the file.
#	reverse:	As descibed in the beginning of the file.
#	used.fit:	As descibed in the beginning of the file.
#Description: 	This function will extract fit index data frame and pass to this function for data frame.
#Return: 		vector.c of power of each fit index
#Examples:
#loading.null <- matrix(0, 6, 1)
#loading.null[1:6, 1] <- NA
#LX.NULL <- matrix.object(loading.null, 0.7)
#PH.NULL <- sym.matrix.object(diag(1))
#TD <- sym.matrix.object(diag(6))
#CFA.Model.NULL <- matrix.CFA.object(LY = LX.NULL, PS = PH.NULL, TE = TD)
#SimData.NULL <- data.object(500, CFA.Model.NULL)
#SimModel <- model.object(CFA.Model.NULL)
#Output.NULL <- result.object(SimData.NULL, SimModel, 50)
#Cut.NULL <- find.cutoff(Output.NULL, 0.95)
#u79 <- runif.object(0.7, 0.9)
#loading.alt <- matrix(0, 6, 2)
#loading.alt[1:3, 1] <- NA
#loading.alt[4:6, 2] <- NA
#LX.ALT <- matrix.object(loading.alt, 0.7)
#latent.cor.alt <- matrix(NA, 2, 2)
#diag(latent.cor.alt) <- 1
#PH.ALT <- sym.matrix.object(latent.cor.alt, "u79")
#CFA.Model.ALT <- matrix.CFA.object(LY = LX.ALT, PS = PH.ALT, TE = TD)
#SimData.ALT <- data.object(500, CFA.Model.ALT)
#Output.ALT <- result.object(SimData.ALT, SimModel, 50)
#find.power(Output.ALT, Cut.NULL)
#Rule.of.thumb <- c(RMSEA=0.05, CFI=0.95, TLI=0.95, SRMR=0.06)
#find.power(Output.ALT, Rule.of.thumb, used.fit=c("RMSEA", "CFI", "TLI", "SRMR"))

setMethod("find.power", signature(object.alt="matrix"), definition=function(object.alt, cutoff, reverse=FALSE, used.fit=NULL) {
	object <- as.data.frame(object.alt)
	output <- find.power(object, cutoff, reverse, used.fit)
	return(output)	
})
#Arguments: 
#	object.alt:		data.frame.c that users wish find power from
# 	cutoff:		As descibed in the beginning of the file.
#	reverse:	As descibed in the beginning of the file.
#	used.fit:	As descibed in the beginning of the file.
#Description: 	This function will change matrix.c to data.frame.c and pass to this function with data.frame.c
#Return: 		vector.c of power of each fit index

