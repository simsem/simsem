# summaryPopulation: Summarize population values behind data generation model

summaryPopulation <- function(object) {
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
} 
