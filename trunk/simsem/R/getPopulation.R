# getPopulation
# Methods -- simsem package
# Description: 	Extract the population value from an object
# Argument:
#	object:		SimDataOut/SimModelOut/SimResult class to be extracted the population values from
# Return: 	Depends on the input object
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)

setMethod("getPopulation", signature(object="SimResult"), definition=function(object) {
	return(object@paramValue)
})
#Return: 		data.frame of parameter values in each replication

setMethod("getPopulation", signature(object="SimModelOut"), definition=function(object) {
	return(object@paramValue)
})
#Return: 		SimRSet of parameter values

setMethod("getPopulation", signature(object="SimDataOut"), definition=function(object, misspec=TRUE) {
	if(misspec) {
		return(object@misspecOut)
	} else {
		return(object@paramOut)	
	}
})
#Additional Arguments: 
#	misspec:	If TRUE, provide the parameter with misspecification. If FALSE, provide the parameter without misspecification.
#Return: 		SimRSet of parameter values
