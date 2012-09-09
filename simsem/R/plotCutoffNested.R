# plotCutoffNested: plot the cutoff from the difference in fit indices from two
# models

plotCutoffNested <- function(nested, parent, alpha = 0.05, cutoff = NULL, usedFit = NULL, 
    useContour = T) {
    mod <- clean(nested, parent)
    nested <- mod[[1]]
    parent <- mod[[2]]
    if (!all.equal(unique(nested@paramValue), unique(parent@paramValue))) 
        stop("Models are based on different data and cannot be compared, check your random seed")
    if (!all.equal(unique(nested@n), unique(parent@n))) 
        stop("Models are based on different values of sample sizes")
    if (!all.equal(unique(nested@pmMCAR), unique(parent@pmMCAR))) 
        stop("Models are based on different values of the percent completely missing at random")
    if (!all.equal(unique(nested@pmMAR), unique(parent@pmMAR))) 
        stop("Models are based on different values of the percent missing at random")
    
    Data <- as.data.frame(nested@fit - parent@fit)
    
    condition <- c(length(unique(nested@pmMCAR)) > 1, length(unique(nested@pmMAR)) > 
        1, length(unique(nested@n)) > 1)
    condValue <- cbind(nested@pmMCAR, nested@pmMAR, nested@n)
    colnames(condValue) <- c("Percent MCAR", "Percent MAR", "N")
    if (!is.null(alpha)) {
        if (all(!condition)) 
            if (is.null(cutoff)) 
                cutoff <- getCutoff(Data, alpha)
    }
    if (sum(condition) == 0) {
        plotCutoff(Data, cutoff, FALSE, usedFit)
    } else if (sum(condition) == 1) {
        plotCutoff(Data, cutoff, FALSE, usedFit, vector1 = condValue[, condition], 
            nameVector1 = colnames(condValue)[condition], alpha = alpha)
    } else if (sum(condition) == 2) {
        condValue <- condValue[, condition]
        plotCutoff(Data, cutoff, FALSE, usedFit, vector1 = condValue[, 1], vector2 = condValue[, 
            2], nameVector1 = colnames(condValue)[1], nameVector2 = colnames(condValue)[2], 
            alpha = alpha, useContour = useContour)
    } else {
        stop("This function cannot plot when there more than two dimensions of varying parameters")
    }
} 
