# plotCutoffNonNested: plot the cutoff from the difference in fit indices from
# two models

plotCutoffNonNested <- function(dat1Mod1, dat1Mod2, dat2Mod1 = NULL, dat2Mod2 = NULL, 
    alpha = 0.05, cutoff = NULL, usedFit = NULL, useContour = T, onetailed = FALSE) {
    mod1 <- clean(dat1Mod1, dat1Mod2)
    dat1Mod1 <- mod1[[1]]
    dat1Mod2 <- mod1[[2]]
	usedFit <- cleanUsedFit(usedFit, colnames(dat1Mod1@fit), colnames(dat1Mod2@fit))
	
    if (!isTRUE(all.equal(unique(dat1Mod1@paramValue), unique(dat1Mod2@paramValue)))) 
        stop("'dat1Mod1' and 'dat1Mod2' are based on different data and cannot be compared, check your random seed")
    if (!is.null(dat2Mod1) & !is.null(dat2Mod2)) {
        mod2 <- clean(dat2Mod1, dat2Mod2)
        dat2Mod1 <- mod2[[1]]
        dat2Mod2 <- mod2[[2]]
        if (!isTRUE(all.equal(unique(dat2Mod1@paramValue), unique(dat2Mod2@paramValue)))) 
            stop("'dat2Mod1' and 'dat2Mod2' are based on different data and cannot be compared, check your random seed")
        if (!multipleAllEqual(unique(dat1Mod1@n), unique(dat1Mod2@n), unique(dat2Mod1@n), 
            unique(dat2Mod2@n))) 
            stop("Models are based on different values of sample sizes")
        if (!multipleAllEqual(unique(dat1Mod1@pmMCAR), unique(dat1Mod2@pmMCAR), unique(dat2Mod1@pmMCAR), 
            unique(dat2Mod2@pmMCAR))) 
            stop("Models are based on different values of the percent completely missing at random")
        if (!multipleAllEqual(unique(dat1Mod1@pmMAR), unique(dat1Mod2@pmMAR), unique(dat2Mod1@pmMAR), 
            unique(dat2Mod2@pmMAR))) 
            stop("Models are based on different values of the percent missing at random")
    } else {
        if (!isTRUE(all.equal(unique(dat1Mod1@n), unique(dat1Mod2@n)))) 
            stop("Models are based on different values of sample sizes")
        if (!isTRUE(all.equal(unique(dat1Mod1@pmMCAR), unique(dat1Mod2@pmMCAR)))) 
            stop("Models are based on different values of the percent completely missing at random")
        if (!isTRUE(all.equal(unique(dat1Mod1@pmMAR), unique(dat1Mod2@pmMAR)))) 
            stop("Models are based on different values of the percent missing at random")
    }
    
    Data1 <- as.data.frame((dat1Mod1@fit - dat1Mod2@fit))
    Data2 <- NULL
    if (!is.null(dat2Mod1) & !is.null(dat2Mod2)) 
        Data2 <- as.data.frame((dat2Mod1@fit - dat2Mod2@fit))
    
    condition <- c(length(unique(dat1Mod1@pmMCAR)) > 1, length(unique(dat1Mod1@pmMAR)) > 
        1, length(unique(dat1Mod1@n)) > 1)
    condValue <- cbind(dat1Mod1@pmMCAR, dat1Mod1@pmMAR, dat1Mod1@n)
    colnames(condValue) <- c("Percent MCAR", "Percent MAR", "N")
    cutoff1 <- cutoff
    cutoff2 <- NULL
    cutoff3 <- NULL
    if (!is.null(cutoff)) 
        cutoff3 <- -cutoff
    cutoff4 <- NULL
    if (!is.null(alpha)) {
        if (all(!condition)) {
            if (is.null(cutoff)) {
                cutoff <- getCutoffNonNested(dat1Mod1, dat1Mod2, alpha = alpha, onetailed = onetailed)[[1]]
                cutoff1 <- cutoff[1, ]
                cutoff2 <- cutoff[2, ]
                if (!is.null(dat2Mod1) && !is.null(dat2Mod2)) {
                  cutoff <- getCutoffNonNested(dat2Mod2, dat2Mod1, alpha = alpha, 
                    onetailed = onetailed)[[1]]
                  cutoff3 <- -cutoff[1, ]
                  cutoff4 <- -cutoff[2, ]
                }
            }
        }
    }
    if (sum(condition) == 0) {
        if (!is.null(dat2Mod1) && !is.null(dat2Mod2)) {
            plotOverHist(Data2, Data1, cutoff = cutoff1, usedFit = usedFit, cutoff2 = cutoff2, 
                cutoff3 = cutoff3, cutoff4 = cutoff4)
        } else {
            plotCutoff(Data1, cutoff1, usedFit = usedFit, cutoff2 = cutoff2)
        }
    } else if (sum(condition) == 1) {
        plotCutoff(Data2, cutoff, FALSE, usedFit, vector1 = condValue[, condition], 
            nameVector1 = colnames(condValue)[condition], alpha = alpha)
    } else if (sum(condition) == 2) {
        condValue <- condValue[, condition]
        plotCutoff(Data2, cutoff, FALSE, usedFit, vector1 = condValue[, 1], vector2 = condValue[, 
            2], nameVector1 = colnames(condValue)[1], nameVector2 = colnames(condValue)[2], 
            alpha = alpha, useContour = useContour)
    } else {
        stop("This function cannot plot when there more than two dimensions of varying parameters")
    }
} 
