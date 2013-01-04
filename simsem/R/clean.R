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

cleanMultiple <- function(..., improper = FALSE) {
    object.l <- list(...)
	paramOnly <- sapply(object.l, function(x) x$paramOnly)
	if(all(!paramOnly)) {
		converged <- sapply(object.l, function(x) x$converged)
		targetRep <- 0
		if(improper) targetRep <- c(0, 3:5)
		allConverged <- matrix(converged %in% targetRep, nrow(converged), ncol(converged))
		allConverged <- apply(allConverged, 1, all)

		if (all(!allConverged)) 
			stop("All replications in the result object are not convergent. Thus, the result object cannot be used.")
		object.l <- lapply(object.l, cleanSingle, converged = allConverged, improper = improper)
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

cleanSingle <- function(object, converged = NULL, improper = FALSE) {
    object$clean(converged = converged, improper = improper)
} 
