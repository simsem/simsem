# clean: Extract only simultaneous converged replications in the result objects

# \title{
	# Extract only converged replications in the result objects
# }
# \description{
	# Extract only the replications that are converegent in all supplied result objects (\code{\linkS4class{SimResult}})
# }
# \usage{
# clean(...)
# }
# \arguments{
  # \item{\dots}{
	# The target result objects (\code{\linkS4class{SimResult}})
# }
# }
# \value{
	# The cleaned result objects
# }

clean <- function(..., improper = FALSE) {
    object.l <- list(...)
	paramOnly <- sapply(object.l, slot, name = "paramOnly")
	if(all(!paramOnly)) {
		converged <- sapply(object.l, slot, name = "converged")
		if(!is.matrix(converged)) converged <- as.matrix(converged)
		targetRep <- 0
		if(improper) targetRep <- c(0, 3:7)
		allConverged <- matrix(converged %in% targetRep, nrow(converged), ncol(converged))
		allConverged <- apply(allConverged, 1, all)

		if (all(!allConverged)) 
			stop("All replications in the result object are not convergent. Thus, the result object cannot be used.")
		object.l <- lapply(object.l, cleanSimResult, converged = allConverged, improper = improper)
	}
	if (length(object.l) == 1) 
		object.l <- object.l[[1]]
    return(object.l)
}

# cleanSimResult: Extract only converged replications in a result object

# \title{
	# Extract only converged replications in the result object
# }
# \description{
	# Extract only the replications that are converegent in a result object (\code{\linkS4class{SimResult}})
# }
# \usage{
# cleanSimResult(object, converged=NULL)
# }
# \arguments{
  # \item{object}{
	# The target result object (\code{\linkS4class{SimResult}})
# }
  # \item{converged}{
	# The replications to be extracted. If \code{NULL}, the converged slot in the result object will be used
# }
# }
# \value{
	# The cleaned result object
# }

cleanSimResult <- function(object, converged = NULL, improper = FALSE) {
    if (is.null(converged)) {
		targetRep <- 0
		if(improper) targetRep <- c(0, 3:7)
        converged <- object@converged %in% targetRep
	}
    object@nRep <- sum(converged)
    object@coef <- object@coef[converged, , drop=FALSE]
    object@se <- object@se[converged, , drop=FALSE]
    object@fit <- object@fit[converged, , drop=FALSE]
    if (!is.null(object@paramValue) && (nrow(object@paramValue) > 1)) 
        object@paramValue <- object@paramValue[converged, , drop=FALSE]
    if (!is.null(object@misspecValue) && (nrow(object@misspecValue) > 1)) 
        object@misspecValue <- object@misspecValue[converged, , drop=FALSE]
    if (!is.null(object@popFit) && (nrow(object@popFit) > 1)) 
        object@popFit <- object@popFit[converged, , drop=FALSE]
    if (!is.null(object@extraOut) && (length(object@extraOut) > 1)) 
        object@extraOut <- object@extraOut[converged]
    if (!is.null(object@FMI1)) 
        object@FMI1 <- object@FMI1[converged, , drop=FALSE]
    if (!is.null(object@FMI2)) 
        object@FMI2 <- object@FMI2[converged, , drop=FALSE]
    if (!is.null(object@cilower)) 
        object@cilower <- object@cilower[converged, , drop=FALSE]
    if (!is.null(object@ciupper)) 
        object@ciupper <- object@ciupper[converged, , drop=FALSE]
    object@stdCoef <- object@stdCoef[converged, , drop=FALSE]
    object@stdSe <- object@stdSe[converged, , drop=FALSE]
    object@seed <- object@seed
    if (length(object@n) > 1) 
        object@n <- object@n[converged]
    if (length(object@pmMCAR) > 1) 
        object@pmMCAR <- object@pmMCAR[converged]
    if (length(object@pmMAR) > 1) 
        object@pmMAR <- object@pmMAR[converged]
    object@converged <- object@converged[converged]
    return(object)
} 
