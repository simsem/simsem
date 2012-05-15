# plotPower: This function will plot sampling distributions of fit indices that visualize power

setMethod("plotPower", signature(altObject = "data.frame", nullObject = "vector"), definition = function(altObject, nullObject, usedFit = NULL) {
    plotCutoff(altObject, nullObject, usedFit = usedFit)
})

setMethod("plotPower", signature(altObject = "SimResult", nullObject = "vector"), definition = function(altObject, nullObject, usedFit = NULL) {
    altObject <- clean(altObject)
    plotCutoff(altObject@fit, nullObject, usedFit = usedFit)
})

setMethod("plotPower", signature(altObject = "data.frame", nullObject = "data.frame"), definition = function(altObject, nullObject, alpha, usedFit = NULL) {
    percentile <- 1 - alpha
    cutoff <- getCutoff(nullObject, alpha, usedFit = usedFit)
    names(cutoff) <- usedFit
    if (is.null(usedFit)) 
        usedFit <- getKeywords()$usedFit
    altObject <- as.data.frame(altObject[, usedFit])
    nullObject <- as.data.frame(nullObject[, usedFit])
    colnames(altObject) <- usedFit
    colnames(nullObject) <- usedFit
    no.NA.altObject <- !apply(altObject, 2, function(vec) all(is.na(vec)))
    no.NA.nullObject <- !apply(nullObject, 2, function(vec) all(is.na(vec)))
    temp.name.alt <- colnames(altObject)[no.NA.altObject]
    temp.name.null <- colnames(nullObject)[no.NA.nullObject]
    altObject <- as.data.frame(altObject[, no.NA.altObject])
    nullObject <- as.data.frame(nullObject[, no.NA.nullObject])
    colnames(altObject) <- temp.name.alt
    colnames(nullObject) <- temp.name.null
    common.name <- intersect(colnames(altObject), colnames(nullObject))
    altObject <- as.data.frame(altObject[, common.name])
    nullObject <- as.data.frame(nullObject[, common.name])
    colnames(altObject) <- colnames(nullObject) <- common.name
    cutoff <- cutoff[common.name]
    if (length(common.name) == 2) {
        obj <- par(mfrow = c(1, 2))
    } else if (length(common.name) == 3) {
        obj <- par(mfrow = c(1, 3))
    } else if (length(common.name) > 3) {
        obj <- par(mfrow = c(2, ceiling(length(common.name)/2)))
    } else if (length(common.name) == 1) {
        # Intentionally leaving as blank
    } else {
        stop("Some errors occur")
    }
    for (i in 1:length(common.name)) {
        swap <- sum(common.name[i] == c("CFI", "TLI")) > 0
        overlapHist(nullObject[, i], altObject[, i], main = common.name[i], xlab = "Value", colors = c("yellow", "skyblue", "lightgreen"), swap = swap)
        cutoff1 <- quantile(nullObject[, i], percentile, na.rm = TRUE)
        abline(v = cutoff[i], lty = 1, lwd = 3)
        position <- "topright"
        if (swap) 
            position <- "topleft"
        legend(position, c("Null", "Alternative"), cex = 1, bty = "n", fill = c("yellow", "skyblue"))
    }
    if (length(common.name) > 1) 
        par(obj)
})

setMethod("plotPower", signature(altObject = "SimResult", nullObject = "SimResult"), definition = function(altObject, nullObject, alpha, usedFit = NULL) {
    altObject <- clean(altObject)
    nullObject <- clean(nullObject)
    plotPower(altObject@fit, nullObject@fit, alpha, usedFit)
}) 
