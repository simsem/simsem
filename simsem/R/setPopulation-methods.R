# setPopulation: Set population parameter values

setMethod("setPopulation", signature(target = "SimResult", population = "data.frame"), definition = function(target, population) {
    target@paramValue <- population
    return(target)
})

setMethod("setPopulation", signature(target = "SimResult", population = "SimSet"), definition = function(target, population) {
    LabelsDataParam <- makeLabels(createFreeParameters(population), "OpenMx")
    pop <- startingValues(population, 10, reduced = TRUE)
    target@paramValue <- as.data.frame(t(data.frame(param = vectorizeObject(pop, LabelsDataParam))))
    return(target)
})

setMethod("setPopulation", signature(target = "SimResult", population = "VirtualRSet"), definition = function(target, population, parameter) {
    LabelsDataParam <- makeLabels(parameter, "OpenMx")
    target@paramValue <- vectorizeObject(population, LabelsDataParam)
    return(target)
})

setMethod("setPopulation", signature(target = "SimModelOut", population = "SimRSet"), definition = function(target, population) {
    target@paramValue <- population
    return(target)
})

setMethod("setPopulation", signature(target = "SimModelOut", population = "SimSet"), definition = function(target, population) {
    pop <- startingValues(population, 10, reduced = TRUE)
    target@paramValue <- pop
    return(target)
}) 
