# summaryPopulation: Summarize population values behind data generation model

setMethod("summaryPopulation", signature(object = "SimResult"), definition = function(object) {
    object <- clean(object)
    paramValue <- object@paramValue
    nRep <- nrow(paramValue)
    nParam <- ncol(paramValue)
    result <- NULL
    if (nrow(object@paramValue) == 1) {
        result <- matrix(paramValue, nrow = 1)
        rownames(result) <- "Population Value"
    } else {
        average.param <- apply(paramValue, 2, mean, na.rm = TRUE)
        sd.param <- apply(paramValue, 2, sd, na.rm = TRUE)
        result <- rbind(average.param, sd.param)
        rownames(result) <- c("Average", "SD")
    }
    return(result)
})

setMethod("summaryPopulation", signature(object = "SimModelOut"), definition = function(object) {
    ifelse(isNullObject(object@paramValue), print("There is no parameter value underlying the data."), summary(object@paramValue))
})

setMethod("summaryPopulation", signature(object = "SimDataOut"), definition = function(object) {
    cat("======== Real Parameters =========\n")
    summary(object@paramOut)
    if (!isNullObject(object@misspecOut)) {
        cat("======== Real Parameters with Model Misspecification ========\n")
        summary(object@misspecOut)
    }
}) 
