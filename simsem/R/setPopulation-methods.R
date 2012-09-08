# setPopulation: Set population parameter values

# setMethod("setPopulation", signature(target = "SimResult", population = "data.frame"), definition = function(target, population) {
    # target@paramValue <- population
    # return(target)
# })


setPopulation <- function(target, population) {
	psl <- generate(population, n=20, params=TRUE)$psl
	paramSet <- lapply(psl,"[[",1)
	indLabGen <- NULL
	if(population@modelType == "Path") {
		indLabGen <- unique(population@pt$lhs)
	} else {
		indLabGen <- unique(population@pt$rhs[population@pt$op=="=~"])
	}
	facLabGen <- NULL
	if(population@modelType != "Path") {
		facLabGen <- unique(population@pt$lhs[population@pt$op=="=~"])
	}
	popParam <- reduceParamSet(paramSet,population@dgen, indLabGen, facLabGen)
    target@paramValue <- as.data.frame(t(data.frame(param = popParam)))
    return(target)
}

# setMethod("setPopulation", signature(target = "SimResult", population = "VirtualRSet"), definition = function(target, population, 
    # parameter) {
    # LabelsDataParam <- makeLabels(parameter, "OpenMx")
    # target@paramValue <- vectorizeObject(population, LabelsDataParam)
    # return(target)
# })

## setMethod("setPopulation", signature(target = "SimModelOut", population = "SimRSet"), definition = function(target, population) {
##     target@paramValue <- population
##     return(target)
## })

## setMethod("setPopulation", signature(target = "SimModelOut", population = "SimSet"), definition = function(target, population) {
##     pop <- startingValues(population, 10, reduced = TRUE)
##     target@paramValue <- pop
##     return(target)
## }) 
