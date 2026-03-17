### Sunthud Pornprasertmanit & Terrence D. Jorgensen
### Last updated: 6 March 2026
### Fit index p-value utilities used in dynamic cutoff evaluation for a non-nested model comparison

#' Find p-values (1 - percentile) for a non-nested model comparison
#'
#' This function provides \emph{p}-values by comparing the results of fitting
#' real data to two competing models against simulation results obtained by
#' fitting simulated data from both models to both models. The \emph{p}-values
#' are computed using sampling distributions obtained under the datasets
#' generated from each model.
#'
#' @param outMod1 A \code{lavaan}-class object that saves the analysis result
#' of the first model from the target dataset.
#' @param outMod2 A \code{lavaan}-class object that saves the analysis result
#' of the second model from the target dataset.
#' @param dat1Mod1 A \code{\linkS4class{SimResult}} object containing the
#' simulation results from analyzing Model 1 using datasets generated from Model 1.
#' @param dat1Mod2 A \code{\linkS4class{SimResult}} object containing the
#' simulation results from analyzing Model 2 using datasets generated from Model 1.
#' @param dat2Mod1 A \code{\linkS4class{SimResult}} object containing the
#' simulation results from analyzing Model 1 using datasets generated from Model 2.
#' @param dat2Mod2 A \code{\linkS4class{SimResult}} object containing the
#' simulation results from analyzing Model 2 using datasets generated from Model 2.
#' @param usedFit Vector of names of fit indices that researchers wish to obtain
#' cutoffs from. The default is to use all available fit indices.
#' @param nVal The sample size value for which researchers wish to compute the
#' \emph{p}-value.
#' @param pmMCARval The percent missing completely at random value for which
#' researchers wish to compute the \emph{p}-value.
#' @param pmMARval The percent missing at random value for which researchers
#' wish to compute the \emph{p}-value.
#' @param df The degree of freedom used in spline methods for predicting fit
#' indices by predictors. If \code{df = 0}, the spline method is not applied.
#' @param onetailed Logical indicating whether a one-tailed test should be
#' used. If \code{FALSE}, two-tailed \emph{p}-values are returned.
#'
#' @return
#' A list containing two vectors of \emph{p}-values:
#'
#' \itemize{
#' \item \code{pValueMod1}: p-values based on the sampling distribution when
#' Model 1 generated the data.
#' \item \code{pValueMod2}: p-values based on the sampling distribution when
#' Model 2 generated the data.
#' }
#'
#' Each vector contains the \emph{p}-values of the requested fit indices along
#' with two additional summary values:
#'
#' \itemize{
#' \item \code{andRule}: The proportion of replications in which all fit indices
#' indicate a better model than the observed data. This represents the most
#' stringent rule for retaining a hypothesized model.
#'
#' \item \code{orRule}: The proportion of replications in which at least one fit
#' index indicates a better model than the observed data. This represents the
#' most lenient rule for retaining a hypothesized model.
#' }
#'
#' @details
#' In comparing fit indices, the \emph{p}-value is the proportion of replications
#' that provide less preference for either Model 1 or Model 2 than the analysis
#' result obtained from the observed data.
#'
#' In a two-tailed test, the function reports the proportion of values in the
#' sampling distribution that are more extreme than the value obtained from the
#' real data. If the resulting \emph{p}-value is high (> .05) for one model and
#' low (< .05) for the other model, the model with the higher \emph{p}-value is
#' preferred. If both \emph{p}-values are either high or low, the decision is
#' considered undetermined.
#'
#' @seealso
#' \code{\linkS4class{SimResult}}
#'
#' @examples
#' \dontrun{
#' # Model A; Factor 1 --> Factor 2; Factor 2 --> Factor 3
#' library(lavaan)
#' loading <- matrix(0, 11, 3)
#' loading[1:3, 1] <- NA
#' loading[4:7, 2] <- NA
#' loading[8:11, 3] <- NA
#' path.A <- matrix(0, 3, 3)
#' path.A[2, 1] <- NA
#' path.A[3, 2] <- NA
#' model.A <- estmodel(LY=loading, BE=path.A, modelType="SEM",
#'     indLab=c(paste("x", 1:3, sep=""), paste("y", 1:8, sep="")))
#'
#' out.A <- analyze(model.A, PoliticalDemocracy)
#'
#' # Model B; Factor 1 --> Factor 3; Factor 3 --> Factor 2
#' path.B <- matrix(0, 3, 3)
#' path.B[3, 1] <- NA
#' path.B[2, 3] <- NA
#' model.B <- estmodel(LY=loading, BE=path.B, modelType="SEM",
#'     indLab=c(paste("x", 1:3, sep=""), paste("y", 1:8, sep="")))
#'
#' out.B <- analyze(model.B, PoliticalDemocracy)
#'
#' loading.mis <- matrix("runif(1, -0.2, 0.2)", 11, 3)
#' loading.mis[is.na(loading)] <- 0
#'
#' datamodel.A <- model.lavaan(out.A, std=TRUE, LY=loading.mis)
#' datamodel.B <- model.lavaan(out.B, std=TRUE, LY=loading.mis)
#'
#' n <- nrow(PoliticalDemocracy)
#'
#' output.A.A <- sim(20, n=n, model.A, generate=datamodel.A)
#' output.A.B <- sim(20, n=n, model.B, generate=datamodel.A)
#' output.B.A <- sim(20, n=n, model.A, generate=datamodel.B)
#' output.B.B <- sim(20, n=n, model.B, generate=datamodel.B)
#'
#' pValueNonNested(out.A, out.B,
#'                 output.A.A, output.A.B,
#'                 output.B.A, output.B.B)
#'
#' # If the p-value for model A is significant but the p-value for model B
#' # is not significant, model B is preferred.
#' }
#'
#' @export
pValueNonNested <- function(outMod1, outMod2, dat1Mod1, dat1Mod2, dat2Mod1, dat2Mod2,
    usedFit = NULL, nVal = NULL, pmMCARval = NULL, pmMARval = NULL, df = 0, onetailed = FALSE) {
    mod1 <- clean(dat1Mod1, dat1Mod2)
    dat1Mod1 <- mod1[[1]]
    dat1Mod2 <- mod1[[2]]
    mod2 <- clean(dat2Mod1, dat2Mod2)
    dat2Mod1 <- mod2[[1]]
    dat2Mod2 <- mod2[[2]]
	usedFit <- cleanUsedFit(usedFit, colnames(dat1Mod1@fit), colnames(dat1Mod2@fit), colnames(dat2Mod1@fit), colnames(dat2Mod2@fit))
    revDirec <- (usedFit %in% getKeywords()$reversedFit)  # CFA --> FALSE, RMSEA --> TRUE

    if (!isTRUE(all.equal(unique(dat2Mod1@paramValue), unique(dat2Mod2@paramValue))))
        stop("'dat2Mod1' and 'dat2Mod2' are based on different data and cannot be compared, check your random seed")
    if (!isTRUE(all.equal(unique(dat1Mod1@paramValue), unique(dat1Mod2@paramValue))))
        stop("'dat1Mod1' and 'dat1Mod2' are based on different data and cannot be compared, check your random seed")
    if (!multipleAllEqual(unique(dat2Mod1@n), unique(dat2Mod2@n), unique(dat1Mod1@n),
        unique(dat1Mod2@n)))
        stop("Models are based on different values of sample sizes")
    if (!multipleAllEqual(unique(dat2Mod1@pmMCAR), unique(dat2Mod2@pmMCAR), unique(dat1Mod1@pmMCAR),
        unique(dat1Mod2@pmMCAR)))
        stop("Models are based on different values of the percent completely missing at random")
    if (!multipleAllEqual(unique(dat2Mod1@pmMAR), unique(dat2Mod2@pmMAR), unique(dat1Mod1@pmMAR),
        unique(dat1Mod2@pmMAR)))
        stop("Models are based on different values of the percent missing at random")

    if (is.null(nVal) || is.na(nVal))
        nVal <- NULL
    if (is.null(pmMCARval) || is.na(pmMCARval))
        pmMCARval <- NULL
    if (is.null(pmMARval) || is.na(pmMARval))
        pmMARval <- NULL

    condition <- c(length(unique(dat2Mod1@pmMCAR)) > 1, length(unique(dat2Mod1@pmMAR)) >
        1, length(unique(dat2Mod1@n)) > 1)
    condValue <- cbind(dat2Mod1@pmMCAR, dat2Mod1@pmMAR, dat2Mod1@n)
    colnames(condValue) <- c("Percent MCAR", "Percent MAR", "N")
    condValue <- condValue[, condition]
    if (is.null(condValue) || length(condValue) == 0)
        condValue <- NULL
    predictorVal <- rep(NA, 3)
    if (condition[3]) {
        ifelse(is.null(nVal), stop("Please specify the sample size value, 'nVal', because the sample size in the result object is varying"),
            predictorVal[3] <- nVal)
    }
    if (condition[1]) {
        ifelse(is.null(pmMCARval), stop("Please specify the percent of completely missing at random, 'pmMCARval', because the percent of completely missing at random in the result object is varying"),
            predictorVal[1] <- pmMCARval)
    }
    if (condition[2]) {
        ifelse(is.null(pmMARval), stop("Please specify the percent of missing at random, 'pmMARval', because the percent of missing at random in the result object is varying"),
            predictorVal[2] <- pmMARval)
    }
    predictorVal <- predictorVal[condition]

    Data1 <- as.data.frame((dat1Mod1@fit - dat1Mod2@fit)[, usedFit])
    Data2 <- as.data.frame((dat2Mod1@fit - dat2Mod2@fit)[, usedFit])

    if(inherits(outMod1, "MxModel") & inherits(outMod2, "MxModel")) {
		cutoff <- fitMeasuresMx(outMod1)[usedFit] - fitMeasuresMx(outMod2)[usedFit]
	} else if (inherits(outMod1, "lavaan") & inherits(outMod2, "lavaan")) {
	  cutoff <- lavaan::fitMeasures(outMod1, fit.measures = usedFit) -
	            lavaan::fitMeasures(outMod2, fit.measures = usedFit)
	} else if (inherits(outMod1, "lavaan.mi") & inherits(outMod2, "lavaan.mi")) {
	  cutoff <- getMethod("fitMeasures", "lavaan.mi")(outMod1, fit.measures = usedFit) -
	            getMethod("fitMeasures", "lavaan.mi")(outMod2, fit.measures = usedFit)
	} else {
		stop("The 'outMod1' and 'outMod2' arguments must be both lavaan(.mi) objects or MxModel objects.")
	}

    result1 <- NULL
    result2 <- NULL
    if (any(condition)) {
        result1 <- pValueDataFrame(cutoff, Data1, revDirec, x = condValue, xval = predictorVal,
            df = df, asLogical = FALSE)
        names(result1) <- usedFit
        result2 <- pValueDataFrame(cutoff, Data2, !revDirec, x = condValue, xval = predictorVal,
            df = df, asLogical = FALSE)
        names(result2) <- usedFit
    } else {
        logicalMat1 <- pValueDataFrame(cutoff, Data1, revDirec, asLogical = TRUE)
        result1 <- apply(logicalMat1, 2, mean, na.rm = TRUE)
        names(result1) <- usedFit
        andRule1 <- mean(apply(logicalMat1, 1, all), na.rm = TRUE)
        orRule1 <- mean(apply(logicalMat1, 1, any), na.rm = TRUE)
        result1 <- c(result1, andRule = andRule1, orRule = orRule1)
        logicalMat2 <- pValueDataFrame(cutoff, Data2, !revDirec, asLogical = TRUE)
        result2 <- apply(logicalMat2, 2, mean, na.rm = TRUE)
        names(result2) <- usedFit
        andRule2 <- mean(apply(logicalMat2, 1, all), na.rm = TRUE)
        orRule2 <- mean(apply(logicalMat2, 1, any), na.rm = TRUE)
        result2 <- c(result2, andRule = andRule2, orRule = orRule2)
    }
    if (!onetailed) {
        result1 <- twoTailedPValue(result1)
        result2 <- twoTailedPValue(result2)
    }
    return(list(pValueMod1 = result1, pValueMod2 = result2))
}

#' Convert one-tailed p-values to two-tailed p-values
#'
#' Internal helper that converts one-tailed \emph{p}-values into
#' two-tailed \emph{p}-values.
#'
#' @param vec A vector of one-tailed \emph{p}-values.
#'
#' @return A vector of two-tailed \emph{p}-values.
#'
#' @keywords internal
twoTailedPValue <- function(vec) {
    apply(cbind(vec, 1 - vec), 1, min) * 2
}
