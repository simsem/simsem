# clean: Extract only simultaneous converged replications in the result objects


clean <- function(...) {
object.l <- list(...)
converged <- sapply(object.l, slot, name="converged")
allConverged <- apply(converged, 1, all)
if(all(!allConverged)) stop("All replications in the result object are not convergent. Thus, the result object cannot be used.")
object.l <- lapply(object.l, cleanSimResult, converged=allConverged)
if(length(object.l) == 1) object.l <- object.l[[1]]
return(object.l)
}

# cleanSimResult: Extract only converged replications in a result object
cleanSimResult <- function(object, converged=NULL) {
    if(is.null(converged)) converged <- object@converged
    object@nRep <- sum(converged)
    object@coef <- object@coef[converged, ]
    object@se <- object@se[converged, ]
    object@fit <- object@fit[converged, ]
    object@converged <- rep(TRUE, object@nRep)
    if (!is.null(object@paramValue) && (nrow(object@paramValue) > 1)) 
        object@paramValue <- object@paramValue[converged, ]
    if (!is.null(object@misspecValue) && (nrow(object@misspecValue) > 1)) 
        object@misspecValue <- object@misspecValue[converged, ]
    if (!is.null(object@popFit) && (nrow(object@popFit) > 1)) 
        object@popFit <- object@popFit[converged, ]
    if (!is.null(object@extraOut) && (length(object@extraOut) > 1)) 
        object@extraOut <- object@extraOut[converged]
    if (!is.null(object@FMI1)) 
        object@FMI1 <- object@FMI1[converged, ]
    if (!is.null(object@FMI2)) 
        object@FMI2 <- object@FMI2[converged, ]
    object@stdCoef <- object@stdCoef[converged, ]
    object@seed <- object@seed
    if (length(object@n) > 1) 
        object@n <- object@n[converged]
    if (length(object@pmMCAR) > 1) 
        object@pmMCAR <- object@pmMCAR[converged]
    if (length(object@pmMAR) > 1) 
        object@pmMAR <- object@pmMAR[converged]
	object@converged <- rep(TRUE, sum(converged))
    return(object)
} 
