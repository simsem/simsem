# plotMisfit: Plot the misfit of the SimResultParam

plotMisfit <- function(object, usedFit = "default", misParam = NULL) {
    if (usedFit == "default") {
        ifelse(is.null(misParam), usedFit <- c("f0", "rmsea", "srmr"), usedFit <- "rmsea")
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
    if (all(is.numeric(usedFit))) 
        usedFit <- colnames(object@fit)[usedFit]
    if (!is.null(misParam) && all(is.numeric(misParam))) 
        misParam <- colnames(object@misspec)[misParam]
    for (i in 1:dimOut) {
        if (is.null(misParam)) {
            fit <- object@fit[, usedFit[i]]
            hist(fit, main = usedFit[i], breaks = 10, col = "yellow", xlab = "Value")
        } else {
            fit <- object@fit[, usedFit]
            xVal <- object@misspec[, misParam[i]]
            plot(xVal, fit, main = misParam[i], ylab = usedFit, xlab = "Misspecification Value")
            lines(loess.smooth(xVal, fit), col = "red")
        }
    }
    if (dimOut > 1) 
        par(obj)
} 
