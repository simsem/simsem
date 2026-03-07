### Sunthud Pornprasertmanit 
### Last updated: 6 March 2026
### Plot the cutoff from the difference in fit indices from two models

#' Plot cutoff distributions for nested model comparisons
#'
#' Plot sampling distributions of the differences in fit indices between
#' nested and parent models. Optional cutoff values can be added either by
#' specifying an \code{alpha} level or by supplying predefined cutoffs.
#'
#' @param nested A \code{\linkS4class{SimResult}} object containing results
#' from fitting the nested model across replications.
#'
#' @param parent A \code{\linkS4class{SimResult}} object containing results
#' from fitting the parent model across replications.
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
#' @return No value is returned. This function produces plots.
#'
#' @seealso
#' \itemize{
#' \item \code{\linkS4class{SimResult}} for simulation result objects used
#' in this function.
#' \item \code{\link{getCutoffNested}} to compute cutoff values for nested
#' model comparisons.
#' }
#'
#' @examples
#' \dontrun{
#' # Nested model: one factor
#' loading.null <- matrix(0, 6, 1)
#' loading.null[1:6, 1] <- NA
#' LY.NULL <- bind(loading.null, 0.7)
#'
#' RPS.NULL <- binds(diag(1))
#' RTE <- binds(diag(6))
#'
#' CFA.Model.NULL <- model(
#'   LY = LY.NULL,
#'   RPS = RPS.NULL,
#'   RTE = RTE,
#'   modelType = "CFA"
#' )
#'
#' # Parent model: two factors
#' loading.alt <- matrix(0, 6, 2)
#' loading.alt[1:3, 1] <- NA
#' loading.alt[4:6, 2] <- NA
#'
#' LY.ALT <- bind(loading.alt, 0.7)
#'
#' latent.cor.alt <- matrix(NA, 2, 2)
#' diag(latent.cor.alt) <- 1
#'
#' RPS.ALT <- binds(latent.cor.alt, "runif(1, 0.7, 0.9)")
#'
#' CFA.Model.ALT <- model(
#'   LY = LY.ALT,
#'   RPS = RPS.ALT,
#'   RTE = RTE,
#'   modelType = "CFA"
#' )
#'
#' Output.NULL.NULL <- sim(10, n = 500, model = CFA.Model.NULL)
#' Output.NULL.ALT  <- sim(10, n = 500, model = CFA.Model.ALT,
#'                         generate = CFA.Model.NULL)
#'
#' # Plot cutoffs for nested model comparison
#' plotCutoffNested(Output.NULL.NULL, Output.NULL.ALT, alpha = 0.05)
#' }
#'
#' @export
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
                cutoff <- getCutoffDataFrame(Data, alpha)
    }
    if (sum(condition) == 0) {
        plotCutoffDataFrame(Data, cutoff, FALSE, usedFit)
    } else if (sum(condition) == 1) {
        plotCutoffDataFrame(Data, cutoff, FALSE, usedFit, vector1 = condValue[, condition], 
            nameVector1 = colnames(condValue)[condition], alpha = alpha)
    } else if (sum(condition) == 2) {
        condValue <- condValue[, condition]
        plotCutoffDataFrame(Data, cutoff, FALSE, usedFit, vector1 = condValue[, 1], vector2 = condValue[, 
            2], nameVector1 = colnames(condValue)[1], nameVector2 = colnames(condValue)[2], 
            alpha = alpha, useContour = useContour)
    } else {
        stop("This function cannot plot when there more than two dimensions of varying parameters")
    }
} 
