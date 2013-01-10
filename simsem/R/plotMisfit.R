# plotMisfit: Plot the misfit of the SimResult

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
