### Sunthud Pornprasertmanit
### Last updated: 7 March 2026
### Plot confidence interval widths from simulation results

#' Plot confidence interval widths for target parameters
#'
#' Plot the widths of confidence intervals for selected parameters from a
#' \code{\linkS4class{SimResult}} object. The plot can show the distribution
#' of confidence interval widths or how the widths change across varying
#' simulation conditions (e.g., sample size or missing-data rates).
#'
#' The cutoff value represents the width corresponding to a specified
#' assurance level (e.g., the median width when \code{assurance = 0.50}).
#'
#' @param object A \code{\linkS4class{SimResult}} object containing simulation
#' results.
#'
#' @param targetParam Character vector specifying the target parameter(s)
#' whose confidence interval widths should be plotted.
#'
#' @param assurance Percentile of the resulting confidence interval width.
#' For example, \code{assurance = 0.50} corresponds to the median width.
#' See Lai and Kelley (2011) for details.
#'
#' @param useContour Logical indicating whether contour plots should be used
#' when two varying parameters are present. If \code{FALSE}, perspective plots
#' are produced instead.
#'
#' @return No value is returned. This function produces plots.
#'
#' @references
#' Lai, K., & Kelley, K. (2011).
#' Accuracy in parameter estimation for targeted effects in structural
#' equation modeling: Sample size planning for narrow confidence intervals.
#' \emph{Psychological Methods, 16}, 127–148.
#'
#' @seealso
#' \itemize{
#' \item \code{\linkS4class{SimResult}} for simulation result objects.
#' \item \code{\link{getCIwidth}} to compute confidence interval widths.
#' }
#'
#' @examples
#' \dontrun{
#' loading <- matrix(0, 6, 2)
#' loading[1:3, 1] <- NA
#' loading[4:6, 2] <- NA
#'
#' loadingValues <- matrix(0, 6, 2)
#' loadingValues[1:3, 1] <- 0.7
#' loadingValues[4:6, 2] <- 0.7
#'
#' LY <- bind(loading, loadingValues)
#'
#' latent.cor <- matrix(NA, 2, 2)
#' diag(latent.cor) <- 1
#'
#' RPS <- binds(latent.cor, 0.5)
#'
#' error.cor <- matrix(0, 6, 6)
#' diag(error.cor) <- 1
#'
#' RTE <- binds(error.cor)
#'
#' CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType = "CFA")
#'
#' Output <- sim(5, n = 200, model = CFA.Model)
#'
#' # Plot the CI widths of the factor correlation
#' plotCIwidth(Output, "f1~~f2", assurance = 0.80)
#'
#' Output2 <- sim(NULL, n = seq(450, 500, 10), model = CFA.Model)
#'
#' # Plot CI widths along sample size
#' plotCIwidth(Output2, "f1~~f2", assurance = 0.80)
#'
#' Output3 <- sim(NULL,
#'                n = seq(450, 500, 10),
#'                pmMCAR = c(0, 0.05, 0.1, 0.15),
#'                model = CFA.Model)
#'
#' # Plot contour surfaces of CI width
#' plotCIwidth(Output3, "f1~~f2", assurance = 0.80)
#' }
#'
#' @importFrom graphics contour
#' @export
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
