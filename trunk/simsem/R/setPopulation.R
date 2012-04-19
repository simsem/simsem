# setPopulation
# Methods -- simsem package
# Description: 	Set population parameter values
# Argument:
#	target:		SimDataOut/SimModelOut/SimResult class to be set the population values for
#	population:	The new population values
# Return: 	The target object with the new population values
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)

setMethod("setPopulation", signature(target="SimResult", population="data.frame"), definition=function(target, population) {
	target@paramValue <- population
	return(target)
})

setMethod("setPopulation", signature(target="SimResult", population="SimSet"), definition=function(target, population) {
	LabelsDataParam <- makeLabels(createFreeParameters(population), "OpenMx")
	pop <- startingValues(population, 10, reduced=TRUE)
	target@paramValue <- as.data.frame(t(data.frame(param=vectorizeObject(pop, LabelsDataParam))))
	return(target)	
})

setMethod("setPopulation", signature(target="SimResult", population="VirtualRSet"), definition=function(target, population, parameter) {
	LabelsDataParam <- makeLabels(parameter, "OpenMx")
	target@paramValue <- vectorizeObject(population, LabelsDataParam)
	return(target)	
})
# Additional arguments:	parameter is the SimParam class that contains objects of free parameters

setMethod("setPopulation", signature(target="SimModelOut", population="SimRSet"), definition=function(target, population) {
	target@paramValue <- population
	return(target)	
})

setMethod("setPopulation", signature(target="SimModelOut", population="SimSet"), definition=function(target, population) {
	pop <- startingValues(population, 10, reduced=TRUE)
	target@paramValue <- pop
	return(target)	
})
