# plotCutoff: This function will plot sampling distributions of fit indices with vertical lines of cutoffs

setMethod("plotCutoff", signature(object = "data.frame"), definition = function(object, cutoff = NULL, revDirec = FALSE, usedFit = NULL, vector1 = NULL, vector2 = NULL, nameVector1 = NULL, nameVector2 = NULL, alpha = NULL, useContour = T) {
    if (is.null(usedFit)) 
        usedFit <- getKeywords()$usedFit
    object <- as.data.frame(object[, usedFit])
    cutoff <- cutoff[usedFit]
    object <- as.data.frame(object[, !apply(object, 2, function(vec) all(is.na(vec)))])
    colnames(object) <- usedFit
    if (ncol(object) == 2) {
        obj <- par(mfrow = c(1, 2))
    } else if (ncol(object) == 3) {
        obj <- par(mfrow = c(1, 3))
    } else if (ncol(object) > 3) {
        obj <- par(mfrow = c(2, ceiling(ncol(object)/2)))
    } else if (ncol(object) == 1) {
        # Intentionally leaving as blank
    } else {
        stop("Some errors occur")
    }
    for (i in 1:ncol(object)) {
        val <- NULL
        if (!is.null(alpha)) {
            val <- 1 - alpha
            if (usedFit[i] %in% c("CFI", "TLI")) 
                val <- alpha
        }
        if (is.null(vector1) & is.null(vector2)) {
            hist(object[, i], main = colnames(object)[i], breaks = 10, col = "yellow", xlab = "Value")
            if (!is.null(cutoff)) 
                abline(v = cutoff[i], col = "red", lwd = 3)
        } else if (!is.null(vector1) & is.null(vector2)) {
            plotQtile(vector1, object[, i], xlab = nameVector1, ylab = "Value", main = colnames(object)[i], qtile = val, df = 5)
        } else if (!is.null(vector1) & !is.null(vector2)) {
            plot3DQtile(vector1, vector2, object[, i], xlab = nameVector1, ylab = nameVector2, zlab = "Value", main = colnames(object)[i], qtile = val, useContour = useContour, df = 0)
        } else {
            stop("Something is wrong!")
        }
    }
    if (ncol(object) > 1) 
        par(obj)
})

setMethod("plotCutoff", signature(object = "SimResult"), definition = function(object, alpha = NULL, revDirec = FALSE, usedFit = NULL, useContour = T) {
    object <- clean(object)
    cutoff <- NULL
    Data <- as.data.frame(object@fit)

    condition <- c(length(object@pmMCAR) > 1, length(object@pmMAR) > 1, length(object@n) > 1)
    condValue <- cbind(object@pmMCAR, object@pmMAR, object@n)
    colnames(condValue) <- c("Percent MCAR", "Percent MAR", "N")
	if (!is.null(alpha)) {
        if (revDirec) 
            alpha <- 1 - alpha
		if(all(!condition)) cutoff <- getCutoff(Data, alpha)
    }
    if (sum(condition) == 0) {
        plotCutoff(Data, cutoff, revDirec, usedFit)
    } else if (sum(condition) == 1) {
        plotCutoff(Data, cutoff, revDirec, usedFit, vector1 = condValue[, condition], nameVector1 = colnames(condValue)[condition], alpha = alpha)
    } else if (sum(condition) == 2) {
        condValue <- condValue[, condition]
        plotCutoff(Data, cutoff, revDirec, usedFit, vector1 = condValue[, 1], vector2 = condValue[, 2], nameVector1 = colnames(condValue)[1], nameVector2 = colnames(condValue)[2], alpha = alpha, 
            useContour = useContour)
    } else {
        stop("This function cannot plot when there more than two dimensions of varying parameters")
    }
    
}) 
