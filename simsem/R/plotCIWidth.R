
plotCIwidth <- function(object, targetParam, assurance = 0.50, useContour = TRUE) {
    object <- clean(object)
    cutoff <- NULL
    Data <- as.data.frame(object@ciupper - object@cilower)
    
    condition <- c(length(unique(object@pmMCAR)) > 1, length(unique(object@pmMAR)) > 
        1, length(unique(object@n)) > 1)
    condValue <- cbind(object@pmMCAR, object@pmMAR, object@n)
    colnames(condValue) <- c("Percent MCAR", "Percent MAR", "N")
    if (!is.null(assurance)) {
        if (all(!condition)) 
            cutoff <- getCIwidth(object, assurance)
    }
    if (sum(condition) == 0) {
        plotCutoffDataFrame(Data, cutoff, revDirec = FALSE, usedFit = targetParam)
    } else if (sum(condition) == 1) {
        plotCutoffDataFrame(Data, cutoff, revDirec = FALSE, usedFit = targetParam, vector1 = condValue[, condition], 
            nameVector1 = colnames(condValue)[condition], alpha = 1 - assurance)
    } else if (sum(condition) == 2) {
        condValue <- condValue[, condition]
        plotCutoffDataFrame(Data, cutoff, revDirec = FALSE, usedFit = targetParam, vector1 = condValue[, 1], vector2 = condValue[, 
            2], nameVector1 = colnames(condValue)[1], nameVector2 = colnames(condValue)[2], 
            alpha = 1 - assurance, useContour = useContour)
    } else {
        stop("This function cannot plot when there more than two dimensions of varying parameters")
    }
}
