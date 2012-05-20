# clean: Extract only converged replications in the result object

clean <- function(object) {
    converged <- object@converged
    object@nRep <- sum(converged)
    object@coef <- object@coef[converged, ]
    object@se <- object@se[converged, ]
    object@fit <- object@fit[converged, ]
    object@converged <- rep(TRUE, object@nRep)
    if (!isNullObject(object@paramValue) && (nrow(object@paramValue) > 1)) 
        object@paramValue <- object@paramValue[converged, ]
    if (!isNullObject(object@FMI1)) 
        object@FMI1 <- object@FMI1[converged, ]
    if (!isNullObject(object@FMI2)) 
        object@FMI2 <- object@FMI2[converged, ]
    object@stdCoef <- object@stdCoef[converged, ]
    object@seed <- object@seed
    if (length(object@n) > 1) 
        object@n <- object@n[converged]
    if (length(object@pmMCAR) > 1) 
        object@pmMCAR <- object@pmMCAR[converged]
    if (length(object@pmMAR) > 1) 
        object@pmMAR <- object@pmMAR[converged]
    return(object)
} 
