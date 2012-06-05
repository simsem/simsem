# getPopulation: Description: Extract the population value from an object

setMethod("getPopulation", signature(object = "SimResult"), definition = function(object) {
    return(object@paramValue)
})

setMethod("getPopulation", signature(object = "SimModelOut"), definition = function(object) {
    return(object@paramValue)
})

setMethod("getPopulation", signature(object = "SimDataOut"), definition = function(object, 
    misspec = TRUE) {
    if (misspec) {
        return(object@misspecOut)
    } else {
        return(object@paramOut)
    }
}) 
