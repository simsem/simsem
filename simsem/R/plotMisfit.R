### Sunthud Pornprasertmanit 
### Last updated: 6 March 2026
### Plot the misfit of the SimResult

#' Plot population misfit in a simulation result object
#'
#' Plot either the distribution of population misfit or the relationship
#' between misspecified parameters and population misfit (or sample fit indices)
#' from a \code{\linkS4class{SimResult}} object.
#'
#' If no misspecification parameter is specified, the function plots histograms
#' of population misfit indices. If misspecification parameters are specified,
#' the function plots scatterplots showing the relationship between the
#' misspecification values and the selected fit index.
#'
#' @param object A \code{\linkS4class{SimResult}} object.
#'
#' @param usedFit Sample fit indices or population misfit indices used in the
#' plot. All sample fit indices are available. The available population misfit
#' indices are `"pop.f0"`, `"pop.rmsea"`, and `"pop.srmr"`.
#'
#' If \code{misParam} is not specified, all population misfit indices are used.
#' If \code{misParam} is specified, `"pop.rmsea"` is used by default.
#'
#' @param misParam Index or name of misspecified parameters to use in the plot.
#'
#' @return No value is returned. This function produces plots.
#'
#' @examples
#' path.BE <- matrix(0, 4, 4)
#' path.BE[3, 1:2] <- NA
#' path.BE[4, 3] <- NA
#'
#' starting.BE <- matrix("", 4, 4)
#' starting.BE[3, 1:2] <- "runif(1, 0.3, 0.5)"
#' starting.BE[4, 3] <- "runif(1, 0.5, 0.7)"
#'
#' mis.path.BE <- matrix(0, 4, 4)
#' mis.path.BE[4, 1:2] <- "runif(1, -0.1, 0.1)"
#'
#' BE <- bind(path.BE, starting.BE, misspec = mis.path.BE)
#'
#' residual.error <- diag(4)
#' residual.error[1,2] <- residual.error[2,1] <- NA
#' RPS <- binds(residual.error, "rnorm(1, 0.3, 0.1)")
#'
#' ME <- bind(rep(NA, 4), 0)
#'
#' Path.Model <- model(RPS = RPS, BE = BE, ME = ME, modelType = "Path")
#'
#' # In real applications, more replications are recommended
#' Output <- sim(20, n = 500, Path.Model)
#'
#' # Plot distribution of population misfit
#' plotMisfit(Output)
#'
#' # Plot relationship between population RMSEA and misspecified paths
#' plotMisfit(Output, misParam = 1:2)
#'
#' # Plot relationship between sample CFI and misspecified paths
#' plotMisfit(Output, usedFit = "CFI", misParam = 1:2)
#'
#' @seealso
#' \code{\linkS4class{SimResult}}
#'
#' @export
plotMisfit <- function(object, usedFit = "default", misParam = NULL) {
    if (all(dim(object@misspecValue) == 0)) 
        stop("This object does not have any model misspecification.")
    object <- clean(object)
    if (usedFit == "default") {
        ifelse(is.null(misParam), usedFit <- c("pop.f0", "pop.rmsea", "pop.srmr"), 
            usedFit <- "pop.rmsea")
    } else {
		usedFit <- cleanUsedFit(usedFit)
	}
    dimOut <- length(usedFit)
    if (!is.null(misParam)) {
        if (length(usedFit) > 1) 
            stop("The multiple misfit values cannot be used when the misspecification parameter is specified.")
        dimOut <- length(misParam)
    }
    if (dimOut == 2) {
        obj <- par(mfrow = c(1, 2))
    } else if (dimOut == 3) {
        obj <- par(mfrow = c(1, 3))
    } else if (dimOut > 3) {
        obj <- par(mfrow = c(2, ceiling(dimOut/2)))
    } else if (dimOut == 1) {
        # Intentionally leaving as blank
    } else {
        stop("Some errors occur")
    }
    target <- object@popFit
    names(target) <- paste0("pop.", names(target))
    target <- cbind(object@fit, target)
    if (all(is.numeric(usedFit))) 
        usedFit <- colnames(target)[usedFit]
    if (!is.null(misParam) && all(is.numeric(misParam))) 
        misParam <- colnames(object@misspecValue)[misParam]
    for (i in 1:dimOut) {
        if (is.null(misParam)) {
            fit <- target[, usedFit[i]]
            hist(fit, main = usedFit[i], breaks = 10, col = "yellow", xlab = "Value")
        } else {
            fit <- target[, usedFit]
            xVal <- object@misspecValue[, misParam[i]]
            plot(xVal, fit, main = misParam[i], ylab = usedFit, xlab = "Misspecification Value")
            lines(loess.smooth(xVal, fit), col = "red")
        }
    }
    if (dimOut > 1) 
        par(obj)
} 
