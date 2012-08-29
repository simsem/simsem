# getPopulation: Description: Extract the population value from an object

setMethod("getPopulation", signature(object = "SimResult"), definition = function(object) {
    return(object@paramValue)
})
