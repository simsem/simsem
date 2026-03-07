### Sunthud Pornprasertmanit 
### Last updated: 6 March 2026
### Plot sampling distributions of fit indices that visualize power in detecting misspecified models

#' Plot sampling distributions of fit indices that visualize power of rejecting datasets underlying misspecified models
#'
#' This function plots sampling distributions of fit indices that visualize
#' power in rejecting misspecified models.
#'
#' @param altObject A \code{\linkS4class{SimResult}} object saving the simulation
#' results of fitting the hypothesized model when the hypothesized model is
#' \code{FALSE}.
#'
#' @param nullObject A \code{\linkS4class{SimResult}} object saving the simulation
#' results of fitting the hypothesized model when the hypothesized model is
#' \code{TRUE}. This argument may not be specified if the \code{cutoff} argument
#' is provided.
#'
#' @param cutoff A vector of a priori cutoffs for fit indices.
#'
#' @param usedFit Vector of names of fit indices that researchers wish to plot.
#'
#' @param alpha A priori alpha level.
#'
#' @param contN Include varying sample size in the power plot if available.
#'
#' @param contMCAR Include varying MCAR (missing completely at random percentage)
#' in the power plot if available.
#'
#' @param contMAR Include varying MAR (missing at random percentage) in the
#' power plot if available.
#'
#' @param useContour If two of sample size, percent completely at random, and
#' percent missing at random vary, a 3D plot will be produced. A contour graph
#' is the default. If \code{FALSE}, a perspective plot is used.
#'
#' @param logistic If \code{TRUE} and varying parameters exist (e.g., sample size
#' or percent missing), a logistic regression curve predicting significance from
#' the varying parameters is used. If \code{FALSE}, an overlaying scatterplot
#' with a cutoff line is plotted.
#'
#' @return None. This function only produces plots.
#'
#' @seealso
#' \itemize{
#' \item \code{\linkS4class{SimResult}} for simulation results used in this function.
#' \item \code{\link{getCutoff}} to compute cutoff values based on null hypothesis
#' sampling distributions.
#' \item \code{\link{getPowerFit}} to compute power for rejecting a hypothesized model.
#' }
#'
#' @examples
#' \dontrun{
#' # Null model: One-factor model
#' loading.null <- matrix(0, 6, 1)
#' loading.null[1:6, 1] <- NA
#' LY.NULL <- bind(loading.null, 0.7)
#' RPS.NULL <- binds(diag(1))
#' RTE <- binds(diag(6))
#' CFA.Model.NULL <- model(LY = LY.NULL, RPS = RPS.NULL, RTE = RTE, modelType="CFA")
#'
#' Output.NULL <- sim(50, n=50, model=CFA.Model.NULL, generate=CFA.Model.NULL)
#'
#' # Alternative model: Two-factor model
#' loading.alt <- matrix(0, 6, 2)
#' loading.alt[1:3, 1] <- NA
#' loading.alt[4:6, 2] <- NA
#' LY.ALT <- bind(loading.alt, 0.7)
#' latent.cor.alt <- matrix(NA, 2, 2)
#' diag(latent.cor.alt) <- 1
#' RPS.ALT <- binds(latent.cor.alt, 0.5)
#' CFA.Model.ALT <- model(LY = LY.ALT, RPS = RPS.ALT, RTE = RTE, modelType="CFA")
#'
#' Output.ALT <- sim(50, n=50, model=CFA.Model.NULL, generate=CFA.Model.ALT)
#'
#' plotPowerFit(Output.ALT, nullObject=Output.NULL,
#'              usedFit=c("RMSEA","CFI","TLI","SRMR"))
#' }
#'
#' @export
plotPowerFit <- function(altObject, nullObject = NULL, cutoff = NULL, usedFit = NULL, 
    alpha = 0.05, contN = TRUE, contMCAR = TRUE, contMAR = TRUE, useContour = TRUE, 
    logistic = TRUE) {
    if (is.null(nullObject)) {
        altObject <- clean(altObject)
    } else {
        mod <- clean(altObject, nullObject)
        altObject <- mod[[1]]
        nullObject <- mod[[2]]
        if (!isTRUE(all.equal(unique(altObject@n), unique(nullObject@n)))) 
            stop("Models are based on different values of sample sizes")
        if (!isTRUE(all.equal(unique(altObject@pmMCAR), unique(nullObject@pmMCAR)))) 
            stop("Models are based on different values of the percent completely missing at random")
        if (!isTRUE(all.equal(unique(altObject@pmMAR), unique(nullObject@pmMAR)))) 
            stop("Models are based on different values of the percent missing at random")
    }
    nrep <- dim(altObject@fit)[[1]]
	usedFit <- cleanUsedFit(usedFit, colnames(altObject@fit))
    if (!is.null(cutoff)) {
		names(cutoff) <- cleanUsedFit(names(cutoff))
        usedFit <- intersect(usedFit, names(cutoff))
        cutoff <- cutoff[usedFit]
    }
    # Create matrix of predictors (randomly varying params)
    x <- NULL
    pred <- NULL
    
    if ((length(unique(altObject@n)) > 1) && contN) {
        if (!length(altObject@n) == nrep) {
            stop("Number of random sample sizes is not the same as the number of replications, check to see if N varied across replications")
        }
        x <- cbind(x, N = altObject@n)
        pred$N <- min(altObject@n):max(altObject@n)
    }
    if ((length(unique(altObject@pmMCAR)) > 1) && contMCAR) {
        if (!length(altObject@pmMCAR) == nrep) {
            stop("Number of random pmMCARs is not the same as the number of replications, check to see if pmMCAR varied across replications")
        }
        x <- cbind(x, pmMCAR = altObject@pmMCAR)
        pred$MCAR <- seq(min(altObject@pmMCAR), max(altObject@pmMCAR), by = 0.01)
        
    }
    if ((length(unique(altObject@pmMAR)) > 1) && contMAR) {
        if (!length(altObject@pmMAR) == nrep) {
            stop("Number of random pmMARs is not the same as the number of replications, check to see if pmMAR varied across replications")
        }
        x <- cbind(x, pmMAR = altObject@pmMAR)
        pred$MAR <- seq(min(altObject@pmMAR), max(altObject@pmMAR), by = 0.01)
        
    }
    if (!is.null(nullObject)) 
        nullObject <- nullObject@fit
    plotPowerFitDf(altObject@fit, nullObject = nullObject, cutoff = cutoff, usedFit = usedFit, 
        alpha = alpha, x = x, xval = pred, useContour = useContour, logistic = logistic)
}

#' Plot sampling distributions of fit indices that visualize power of rejecting misspecified models (data.frame version)
#'
#' This function plots sampling distributions of fit indices that visualize
#' power in rejecting misspecified models. This function is similar to
#' \code{\link{plotPowerFit}} but uses \code{data.frame} inputs instead of
#' \code{\linkS4class{SimResult}} objects.
#'
#' @param altObject A \code{data.frame} containing fit-index distributions when
#' the hypothesized model is \code{FALSE}.
#'
#' @param nullObject A \code{data.frame} containing fit-index distributions when
#' the hypothesized model is \code{TRUE}. This argument may be omitted if
#' \code{cutoff} is specified.
#'
#' @param cutoff A vector of a priori cutoffs for fit indices.
#'
#' @param usedFit Vector of names of fit indices to plot.
#'
#' @param alpha A priori alpha level.
#'
#' @param x Predictor values used to evaluate power.
#'
#' @param xval Predictor values for plotting predicted cutoffs.
#'
#' @param useContour If two predictors vary, a contour plot is used by default.
#' If \code{FALSE}, a perspective plot is used.
#'
#' @param logistic Logical indicating whether logistic regression smoothing
#' should be used.
#'
#' @return None. This function only produces plots.
#'
#' @keywords internal
plotPowerFitDf <- function(altObject, nullObject = NULL, cutoff = NULL, usedFit = NULL, 
    alpha = 0.05, x = NULL, xval = NULL, useContour = TRUE, logistic = TRUE) {
    if (is.null(x)) {
        if (is.null(nullObject)) {
            plotCutoffDataFrame(altObject, cutoff, usedFit = usedFit)
        } else {
            plotOverHist(altObject, nullObject, cutoff = cutoff, usedFit = usedFit, 
                alpha = alpha)
            # Plot overlapping histogram; Optional for specifying cutoff or derived cutoff
        }
    } else if (ncol(x) == 1) {
        if (logistic & (!is.null(nullObject) | !is.null(cutoff))) {
            plotLogisticFit(altObject, nullObject = nullObject, cutoff = cutoff, 
                usedFit = usedFit, x = x, xval = xval, alpha = alpha, useContour = useContour)
        } else {
            plotScatter(altObject, nullObject = nullObject, cutoff = cutoff, usedFit = usedFit, 
                x = x, alpha = alpha)
            # Plot scatterplot if only one continuous; Optional for putting horizontal
            # cutoff If the cutoff exists, the power plot can be used.
        }
    } else if (ncol(x) == 2) {
        if (logistic & (!is.null(nullObject) | !is.null(cutoff))) {
            plotLogisticFit(altObject, nullObject = nullObject, cutoff = cutoff, 
                usedFit = usedFit, x = x, xval = xval, alpha = alpha, useContour = useContour)
        } else {
            stop("Cannot make scatter plot with two or more varying variables")
        }
        # If the cutoff exists, the power 2/3D plot can be used.
    } else {
        stop("The varying parameter used cannot be over two dimensions.")
    }
}

#' Plot multiple overlapping histograms
#'
#' Plot multiple overlapping histograms and optionally display cutoff values.
#'
#' @param altObject A \code{data.frame} containing fit-index distributions when
#' the hypothesized model is \code{FALSE}.
#'
#' @param nullObject A \code{data.frame} containing fit-index distributions when
#' the hypothesized model is \code{TRUE}.
#'
#' @param cutoff A vector of a priori cutoffs.
#'
#' @param usedFit Fit indices to plot.
#'
#' @param alpha Significance level used to derive cutoffs if not provided.
#'
#' @param cutoff2 Optional additional cutoff vector.
#'
#' @param cutoff3 Optional cutoff vector applied to alternative distributions.
#'
#' @param cutoff4 Optional additional cutoff vector applied to alternative distributions.
#'
#' @return None. Produces overlapping histograms.
#'
#' @keywords internal
plotOverHist <- function(altObject, nullObject, cutoff = NULL, usedFit = NULL, alpha = 0.05, 
    cutoff2 = NULL, cutoff3 = NULL, cutoff4 = NULL) {
    percentile <- 1 - alpha
	usedFit <- cleanUsedFit(usedFit)
    if (is.null(cutoff)) {
        cutoff <- getCutoffDataFrame(nullObject, alpha, usedFit = usedFit)
        names(cutoff) <- usedFit
    }
    usedFit <- intersect(usedFit, names(cutoff))
    cutoff <- cutoff[usedFit]
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
    if (!is.null(cutoff2)) 
        cutoff2 <- cutoff2[common.name]
    if (!is.null(cutoff3)) 
        cutoff3 <- cutoff3[common.name]
    if (!is.null(cutoff4)) 
        cutoff4 <- cutoff4[common.name]
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
        swap <- sum(common.name[i] == getKeywords()$reversedFit) > 0
        overlapHist(nullObject[, i], altObject[, i], main = common.name[i], xlab = "Value", 
            colors = c("yellow", "skyblue", "lightgreen"), swap = swap)
        abline(v = cutoff[i], lty = 1, lwd = 3, col = "red")
        if (!is.null(cutoff2)) 
            abline(v = cutoff2[i], lty = 1, lwd = 3, col = "red")
        if (!is.null(cutoff3)) 
            abline(v = cutoff3[i], lty = 1, lwd = 3, col = "blue")
        if (!is.null(cutoff4)) 
            abline(v = cutoff4[i], lty = 1, lwd = 3, col = "blue")
        position <- "topright"
        if (swap) 
            position <- "topleft"
        legend(position, c("Null", "Alternative"), cex = 1, bty = "n", fill = c("yellow", 
            "skyblue"))
    }
    if (length(common.name) > 1) 
        par(obj)
}

#' Plot logistic curves predicting rejection of misspecified models
#'
#' This function determines whether a hypothesized model is rejected in each
#' dataset and plots logistic curves predicting rejection probabilities from
#' predictor values.
#'
#' @param altObject A \code{data.frame} containing fit-index distributions when
#' the hypothesized model is \code{FALSE}.
#'
#' @param nullObject A \code{data.frame} containing fit-index distributions when
#' the hypothesized model is \code{TRUE}.
#'
#' @param cutoff A vector of a priori fit-index cutoffs.
#'
#' @param usedFit Fit indices to plot.
#'
#' @param x Predictor values.
#'
#' @param xval Predictor grid values used for prediction.
#'
#' @param alpha Significance level.
#'
#' @param useContour Whether contour plots should be used for 2D predictors.
#'
#' @param df Degrees of freedom for spline predictors.
#'
#' @return None. Produces plots.
#'
#' @keywords internal
plotLogisticFit <- function(altObject, nullObject = NULL, cutoff = NULL, usedFit = NULL, 
    x, xval, alpha = 0.05, useContour = TRUE, df = 0) {
    warnT <- as.numeric(options("warn"))
    options(warn = -1)
    if (is.null(nullObject) & is.null(cutoff)) 
        stop("Please specify the nullObject or cutoff argument")
    if (is.null(usedFit)) 
        usedFit <- getKeywords()$usedFit
    if (!is.null(cutoff)) {
        usedFit <- intersect(usedFit, names(cutoff))
        cutoff <- cutoff[usedFit]
    }
    if (is.null(cutoff)) {
        nullFit <- as.data.frame(nullObject[, usedFit])
        colnames(nullFit) <- usedFit
        temp <- rep(NA, length(usedFit))
        cutoff <- getCutoffDataFrame(object = nullFit, alpha = alpha, revDirec = FALSE, usedFit = usedFit, 
            predictor = x, df = df, predictorVal = "all")
        if (!is.matrix(cutoff)) {
            cutoff <- as.matrix(cutoff)
        }
    }
    if (is.matrix(cutoff)) {
        sig <- as.matrix(altObject[, usedFit] > cutoff)
        colnames(sig) <- usedFit
    } else {
        sig <- mapply(function(dat, x) dat > x, dat = altObject[, usedFit], x = as.list(cutoff))
    }
    reverse <- colnames(sig) %in% getKeywords()$reversedFit
    if (any(reverse)) {
        sig[, reverse] <- !sig[, reverse]
    }
    plotPowerSig(sig, x = x, xval = xval, mainName = usedFit, useContour = useContour)
    options(warn = warnT)
}

#' Plot overlaying scatter plots visualizing rejection power
#'
#' Plot fit-index values against predictor values. The plot may include
#' alternative distributions, null distributions, and cutoff curves.
#'
#' @param altObject A \code{data.frame} containing fit-index distributions when
#' the hypothesized model is \code{FALSE}.
#'
#' @param nullObject A \code{data.frame} containing fit-index distributions when
#' the hypothesized model is \code{TRUE}.
#'
#' @param cutoff A vector of a priori cutoffs.
#'
#' @param usedFit Fit indices to plot.
#'
#' @param x Predictor values.
#'
#' @param alpha Significance level.
#'
#' @param df Degrees of freedom for spline predictors.
#'
#' @return None. Produces scatter plots.
#'
#' @keywords internal
plotScatter <- function(altObject, nullObject = NULL, cutoff = NULL, usedFit = NULL, 
    x, alpha = 0.05, df = 5) {
	usedFit <- cleanUsedFit(usedFit)
    if (!is.null(cutoff)) {
        usedFit <- intersect(usedFit, names(cutoff))
        cutoff <- cutoff[usedFit]
    }
    if (is.null(cutoff) && !is.null(nullObject)) {
        nullFit <- as.data.frame(nullObject[, usedFit])
        colnames(nullFit) <- usedFit
        temp <- rep(NA, length(usedFit))
        cutoff <- getCutoffDataFrame(object = nullFit, alpha = alpha, revDirec = FALSE, usedFit = usedFit, 
            predictor = x, df = df, predictorVal = "all")
        if (!is.matrix(cutoff)) {
            cutoff <- as.matrix(cutoff)
        }
    }
    if (length(usedFit) == 2) {
        obj <- par(mfrow = c(1, 2))
    } else if (length(usedFit) == 3) {
        obj <- par(mfrow = c(1, 3))
    } else if (length(usedFit) > 3) {
        obj <- par(mfrow = c(2, ceiling(length(usedFit)/2)))
    } else if (length(usedFit) == 1) {
        # Intentionally leaving as blank
    } else {
        stop("Some errors occur")
    }
    for (i in 1:length(usedFit)) {
        temp <- NULL
        if (!is.null(cutoff)) {
            if (is.matrix(cutoff)) {
                temp <- cutoff[, i]
            } else {
                temp <- cutoff[i]
            }
        }
        nullVec <- NULL
        if (!is.null(nullObject)) 
            nullVec <- nullObject[, usedFit[i]]
        plotIndividualScatter(altObject[, usedFit[i]], nullVec = nullVec, cutoff = temp, 
            x, main = usedFit[i])
    }
    if (length(usedFit) > 1) 
        par(obj)
}

#' Plot a single overlaying scatter plot visualizing rejection power
#'
#' Plot fit-index values against predictor values, optionally including
#' null distributions and cutoff lines.
#'
#' @param altVec Fit-index values when the hypothesized model is \code{FALSE}.
#'
#' @param nullVec Fit-index values when the hypothesized model is \code{TRUE}.
#'
#' @param cutoff Optional cutoff value.
#'
#' @param x Predictor values.
#'
#' @param main Plot title.
#'
#' @return None. Produces a scatter plot.
#'
#' @keywords internal
plotIndividualScatter <- function(altVec, nullVec = NULL, cutoff = NULL, x, main = NULL) {
    maxAll <- max(c(altVec, nullVec), na.rm = TRUE)
    minAll <- min(c(altVec, nullVec), na.rm = TRUE)
    plot(c(min(x), max(x)), c(minAll, maxAll), type = "n", main = main, xlab = colnames(x), 
        ylab = "Value")
    points(x, altVec, col = "skyblue")
    if (!is.null(nullVec)) 
        points(x, nullVec, col = "black")
    if (is.null(cutoff)) {
        # Intetionally leave as blank
    } else if (length(cutoff) == 1) {
        abline(h = cutoff, col = "red", lwd = 2)
    } else {
        lines(x, cutoff, col = "red", lwd = 2)
    }
} 

#' Plot overlapping histograms
#'
#' Plot two overlapping histograms and highlight overlapping regions.
#'
#' @param a Data for the first histogram.
#'
#' @param b Data for the second histogram.
#'
#' @param colors Colors for the first histogram, second histogram,
#' and overlapping regions.
#'
#' @param breaks Optional break specification.
#'
#' @param xlim Range of the x-axis.
#'
#' @param ylim Range of the y-axis.
#'
#' @param main Plot title.
#'
#' @param xlab X-axis label.
#'
#' @param swap If \code{TRUE}, plot \code{b} first then \code{a}.
#'
#' @return None. Produces histograms.
#'
#' @details
#' Original implementation provided by Chris Miller:
#' \url{http://chrisamiller.com/science/2010/07/20/transparent-overlapping-histograms-in-r/}.
#' Modified by Sunthud Pornprasertmanit.
#'
#' @keywords internal
overlapHist <- function(a, b, colors = c("red", "blue", "purple"), breaks = NULL, 
    xlim = NULL, ylim = NULL, main = NULL, xlab = NULL, swap = FALSE) {
    ahist = NULL
    bhist = NULL
    if (!(is.null(breaks))) {
        ahist = hist(a, breaks = breaks, plot = F)
        bhist = hist(b, breaks = breaks, plot = F)
    } else {
        ahist = hist(a, plot = F)
        bhist = hist(b, plot = F)
        dist = ahist$breaks[2] - ahist$breaks[1]
        mina <- min(ahist$breaks, bhist$breaks)
        maxa <- max(ahist$breaks, bhist$breaks)
        bin <- ceiling((maxa - mina)/dist)
        breaks = seq(mina, maxa, length.out = bin)
        ahist = hist(a, breaks = breaks, plot = F)
        bhist = hist(b, breaks = breaks, plot = F)
    }
    if (is.null(xlim)) {
        xlim = c(min(ahist$breaks, bhist$breaks), max(ahist$breaks, bhist$breaks))
    }
    if (is.null(ylim)) {
        ylim = c(0, max(ahist$counts, bhist$counts))
    }
    overlap = ahist
    for (i in 1:length(overlap$counts)) {
        if (ahist$counts[i] > 0 & bhist$counts[i] > 0) {
            overlap$counts[i] = min(ahist$counts[i], bhist$counts[i])
        } else {
            overlap$counts[i] = 0
        }
    }
    if (swap) {
        plot(bhist, xlim = xlim, ylim = ylim, col = colors[2], main = main, xlab = xlab)
        plot(ahist, xlim = xlim, ylim = ylim, col = colors[1], add = T)
    } else {
        plot(ahist, xlim = xlim, ylim = ylim, col = colors[1], main = main, xlab = xlab)
        plot(bhist, xlim = xlim, ylim = ylim, col = colors[2], add = T)
    }
    plot(overlap, xlim = xlim, ylim = ylim, col = colors[3], add = T)
} 
