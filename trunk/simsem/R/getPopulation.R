# getPopulation
# Methods -- simsem package
# Description: 	Extract the population value from an object
# Argument:
#	object:		SimDataOut/SimModelOut/SimResult class to be extracted the population values from
# Return: 	Depends on the input object
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: April 15, 2011

setMethod("getPopulation", signature(object="SimResult"), definition=function(object) {
	return(object@paramValue)
})

setMethod("getPopulation", signature(object="SimModelOut"), definition=function(object) {
	return(object@paramValue)
})

setMethod("getPopulation", signature(object="SimDataOut"), definition=function(object, misspec=TRUE) {
	if(misspec) {
		return(object@misspecOut)
	} else {
		return(object@paramOut)	
	}
})
