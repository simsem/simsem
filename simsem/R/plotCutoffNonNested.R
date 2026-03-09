### Sunthud Pornprasertmanit 
### Last updated: 6 March 2026
### Plot the cutoff from the difference in fit indices from two models

#' Plot cutoff distributions for non-nested model comparisons
#'
#' Plot sampling distributions of the differences in fit indices between
#' two non-nested models. Optional cutoff values can be added either by
#' specifying an \code{alpha} level or by supplying predefined cutoffs.
#'
#' @param dat1Mod1 A \code{\linkS4class{SimResult}} object containing results
#' from fitting Model 1 to datasets generated from Model 1.
#'
#' @param dat1Mod2 A \code{\linkS4class{SimResult}} object containing results
#' from fitting Model 2 to datasets generated from Model 1.
#'
#' @param dat2Mod1 A \code{\linkS4class{SimResult}} object containing results
#' from fitting Model 1 to datasets generated from Model 2.
#'
#' @param dat2Mod2 A \code{\linkS4class{SimResult}} object containing results
#' from fitting Model 2 to datasets generated from Model 2.
#'
#' @param alpha Significance level used to compute cutoff values.
#'
#' @param cutoff A numeric vector of predefined cutoff values for the
#' differences in fit indices.
#'
#' @param usedFit Character vector specifying which fit indices should be
#' plotted.
#'
#' @param useContour Logical indicating whether contour plots should be used
#' when two varying parameters are present. If \code{FALSE}, perspective plots
#' are produced instead.
#'
#' @param onetailed Logical indicating whether the cutoff should be derived
#' from a one-tailed test. If \code{FALSE}, a two-tailed test is used.
#'
#' @return No value is returned. This function produces plots.
#'
#' @seealso
#' \itemize{
#' \item \code{\linkS4class{SimResult}} for simulation result objects used
#' in this function.
#' \item \code{\link{getCutoffNonNested}} to compute cutoff values for
#' non-nested model comparisons.
#' }
#'
#' @examples
#' \dontrun{
#' # Model A: Factor 1 on Items 1-3 and Factor 2 on Items 4-8
#' loading.A <- matrix(0, 8, 2)
#' loading.A[1:3, 1] <- NA
#' loading.A[4:8, 2] <- NA
#'
#' LY.A <- bind(loading.A, 0.7)
#'
#' latent.cor <- matrix(NA, 2, 2)
#' diag(latent.cor) <- 1
#'
#' RPS <- binds(latent.cor, "runif(1, 0.7, 0.9)")
#' RTE <- binds(diag(8))
#'
#' CFA.Model.A <- model(LY = LY.A, RPS = RPS, RTE = RTE, modelType="CFA")
#'
#' # Model B: Factor 1 on Items 1-4 and Factor 2 on Items 5-8
#' loading.B <- matrix(0, 8, 2)
#' loading.B[1:4, 1] <- NA
#' loading.B[5:8, 2] <- NA
#'
#' LY.B <- bind(loading.B, 0.7)
#'
#' CFA.Model.B <- model(LY = LY.B, RPS = RPS, RTE = RTE, modelType="CFA")
#'
#' Output.A.A <- sim(10, n = 500, model = CFA.Model.A, generate = CFA.Model.A)
#' Output.A.B <- sim(10, n = 500, model = CFA.Model.B, generate = CFA.Model.A)
#' Output.B.A <- sim(10, n = 500, model = CFA.Model.A, generate = CFA.Model.B)
#' Output.B.B <- sim(10, n = 500, model = CFA.Model.B, generate = CFA.Model.B)
#'
#' # Plot cutoffs for both models
#' plotCutoffNonNested(Output.A.A, Output.A.B, Output.B.A, Output.B.B)
#'
#' # Plot cutoffs for Model A only
#' plotCutoffNonNested(Output.A.A, Output.A.B)
#'
#' # Plot cutoffs using a one-tailed test
#' plotCutoffNonNested(Output.A.A, Output.A.B, onetailed = TRUE)
#' }
#'
#' @export
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
            plotCutoffDataFrame(Data1, cutoff1, usedFit = usedFit, cutoff2 = cutoff2)
        }
    } else if (sum(condition) == 1) {
        plotCutoffDataFrame(Data2, cutoff, FALSE, usedFit, vector1 = condValue[, condition], 
            nameVector1 = colnames(condValue)[condition], alpha = alpha)
    } else if (sum(condition) == 2) {
        condValue <- condValue[, condition]
        plotCutoffDataFrame(Data2, cutoff, FALSE, usedFit, vector1 = condValue[, 1], vector2 = condValue[, 
            2], nameVector1 = colnames(condValue)[1], nameVector2 = colnames(condValue)[2], 
            alpha = alpha, useContour = useContour)
    } else {
        stop("This function cannot plot when there more than two dimensions of varying parameters")
    }
} 
