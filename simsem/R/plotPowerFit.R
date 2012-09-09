# plotPowerFit: This function will plot sampling distributions of fit indices
# that visualize power in detecting misspecified models

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
    if (is.null(usedFit)) 
        usedFit <- getKeywords()$usedFit
    if (!is.null(cutoff)) {
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

# plotPowerFitDf: This function will plot sampling distributions of fit indices
# that visualize power in detecting misspecified models where the inputs are
# data.frame

# \title{
# Plot sampling distributions of fit indices that visualize power of rejecting datasets underlying misspecified models
# }
# \description{
# This function will plot sampling distributions of fit indices that visualize power in rejecting the misspecified models. This function is similar to the \code{\link{plotPowerFit}} function but the input distributions are \code{data.frame}.
# }
# \usage{
# plotPowerFitDf(altObject, nullObject = NULL, cutoff = NULL, usedFit = NULL, alpha = 0.05, x = NULL, xval = NULL, useContour = TRUE, logistic = TRUE)
# }
# \arguments{
  # \item{altObject}{
	# The result object (\code{data.frame}) saves the simulation result of fitting the hypothesized model when the hypothesized model is \code{FALSE}.
# }
  # \item{nullObject}{
	# The result object (\code{data.frame}) saves the simulation result of fitting the hypothesized model when the hypothesized model is \code{TRUE}. This argument may be not specified if the \code{cutoff} is specified.
# }
  # \item{cutoff}{
	# A vector of priori cutoffs for fit indices.
# }
  # \item{usedFit}{
	# Vector of names of fit indices that researchers wish to plot.
# }
  # \item{alpha}{
	# A priori alpha level
# }
# \item{x}{
	# The \code{data.frame} of the predictor values. The number of rows of the \code{x} argument should be equal to the number of rows in the \code{object}.
# }
# \item{xval}{
	# The values of predictor that researchers would like to find the fit indices cutoffs from.
# }
  # \item{useContour}{
	# If there are two of sample size, percent completely at random, and percent missing at random are varying, the \code{plotCutoff} function will provide 3D graph. Contour graph is a default. However, if this is specified as \code{FALSE}, perspective plot is used.
# }
  # \item{logistic}{
	# If \code{logistic} is \code{TRUE} and the varying parameter exists (e.g., sample size or percent missing), the plot based on logistic regression predicting the significance by the varying parameters is preferred. If \code{FALSE}, the overlaying scatterplot with a line of cutoff is plotted.
# }
# }
# \value{
	# NONE. Only plot the fit indices distributions.
# }

plotPowerFitDf <- function(altObject, nullObject = NULL, cutoff = NULL, usedFit = NULL, 
    alpha = 0.05, x = NULL, xval = NULL, useContour = TRUE, logistic = TRUE) {
    if (is.null(x)) {
        if (is.null(nullObject)) {
            plotCutoff(altObject, cutoff, usedFit = usedFit)
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

# plotOverHist: Plot overlapping distributions with specified cutoffs

# \title{
# Plot multiple overlapping histograms
# }
# \description{
# Plot multiple overlapping histograms and find the cutoff values if not specified
# }
# \usage{
# plotOverHist(altObject, nullObject, cutoff=NULL, usedFit=NULL, alpha=0.05, 
# cutoff2=NULL, cutoff3=NULL, cutoff4=NULL)
# }
# \arguments{
  # \item{altObject}{
	# The result object (\code{data.frame}) saves the simulation result of fitting the hypothesized model when the hypothesized model is \code{FALSE}.
# }
  # \item{nullObject}{
	# The result object (\code{data.frame}) saves the simulation result of fitting the hypothesized model when the hypothesized model is \code{TRUE}. 
# }
  # \item{cutoff}{
	# A vector of priori cutoffs for fit indices.
# }
  # \item{usedFit}{
	# Vector of names of fit indices that researchers wish to plot.
# }
  # \item{alpha}{
	# A priori alpha level
# }
  # \item{cutoff2}{
	# Another vector of priori cutoffs for fit indices.
# }
  # \item{cutoff3}{
	# A vector of priori cutoffs for fit indices for the \code{altObject}.
# }
  # \item{cutoff4}{
	# Another vector of priori cutoffs for fit indices for the \code{altObject}.
# }
# }
# \value{
	# NONE. Only plot the fit indices distributions.
# }

plotOverHist <- function(altObject, nullObject, cutoff = NULL, usedFit = NULL, alpha = 0.05, 
    cutoff2 = NULL, cutoff3 = NULL, cutoff4 = NULL) {
    percentile <- 1 - alpha
    if (is.null(usedFit)) 
        usedFit <- getKeywords()$usedFit
    if (is.null(cutoff)) {
        cutoff <- getCutoff(nullObject, alpha, usedFit = usedFit)
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
        swap <- sum(common.name[i] == c("CFI", "TLI")) > 0
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

# plotLogisticFit: Plot the logistic curves of predicting the
# rejection/retention of a hypothesized model

# \title{
# Plot multiple logistic curves for predicting whether rejecting a misspecified model
# }
# \description{
# This function will find the fit indices cutoff values if not specified, then check whether the hypothesized model is rejected in each dataset, and plot the logistic curve given the value of predictors. 
# }
# \usage{
# plotLogisticFit(altObject, nullObject=NULL, cutoff=NULL, 
# usedFit=NULL, x, xval, alpha=0.05, useContour=TRUE, df=0)
# }
# \arguments{
  # \item{altObject}{
	# The result object (\code{data.frame}) saves the simulation result of fitting the hypothesized model when the hypothesized model is \code{FALSE}.
# }
  # \item{nullObject}{
	# The result object (\code{data.frame}) saves the simulation result of fitting the hypothesized model when the hypothesized model is \code{TRUE}. This argument may be not specified if the \code{cutoff} is specified.
# }
  # \item{cutoff}{
	# A vector of priori cutoffs for fit indices.
# }
  # \item{usedFit}{
	# Vector of names of fit indices that researchers wish to plot.
# }
  # \item{alpha}{
	# A priori alpha level
# }
# \item{x}{
	# The \code{data.frame} of the predictor values. The number of rows of the \code{x} argument should be equal to the number of rows in the \code{object}.
# }
# \item{xval}{
	# The values of predictor that researchers would like to find the fit indices cutoffs from.
# }
  # \item{useContour}{
	# If there are two of sample size, percent completely at random, and percent missing at random are varying, the \code{plotCutoff} function will provide 3D graph. Contour graph is a default. However, if this is specified as \code{FALSE}, perspective plot is used.
# }
  # \item{df}{
	# The degree of freedom used in spline method in predicting the fit indices by the predictors. If \code{df} is 0, the spline method will not be applied.
# }
# }
# \value{
	# NONE. Only plot the fit indices distributions.
# }

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
        cutoff <- getCutoff(object = nullFit, alpha = alpha, revDirec = FALSE, usedFit = usedFit, 
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
    reverse <- colnames(sig) %in% c("TLI", "CFI")
    if (any(reverse)) {
        sig[, reverse] <- !sig[, reverse]
    }
    plotPowerSig(sig, x = x, xval = xval, mainName = usedFit, useContour = useContour)
    options(warn = warnT)
}

# plotScatter: Plot the overlapping scatterplots showing the distribution of
# fit indices given the values of varying parameters

# \title{
# Plot overlaying scatter plots visualizing the power of rejecting misspecified models
# }
# \description{
# This function will find the fit indices cutoff values if not specified and then plot the fit indices value against the value of predictors. The plot will include the fit indices value of the alternative models, the fit indices value of the null model (if specified), and the fit indices cutoffs.  
# }
# \usage{
# plotScatter(altObject, nullObject=NULL, cutoff=NULL, usedFit = NULL, x, alpha=0.05, df=5)
# }
# \arguments{
  # \item{altObject}{
	# The result object (\code{data.frame}) saves the simulation result of fitting the hypothesized model when the hypothesized model is \code{FALSE}.
# }
  # \item{nullObject}{
	# The result object (\code{data.frame}) saves the simulation result of fitting the hypothesized model when the hypothesized model is \code{TRUE}. This argument may be not specified if the \code{cutoff} is specified.
# }
  # \item{cutoff}{
	# A vector of priori cutoffs for fit indices.
# }
  # \item{usedFit}{
	# Vector of names of fit indices that researchers wish to plot.
# }
# \item{x}{
	# The \code{data.frame} of the predictor values. The number of rows of the \code{x} argument should be equal to the number of rows in the \code{object}.
# }
  # \item{alpha}{
	# A priori alpha level
# }
  # \item{df}{
	# The degree of freedom used in spline method in predicting the fit indices by the predictors. If \code{df} is 0, the spline method will not be applied.
# }
# }
# \value{
	# NONE. Only plot the fit indices distributions.
# }

plotScatter <- function(altObject, nullObject = NULL, cutoff = NULL, usedFit = NULL, 
    x, alpha = 0.05, df = 5) {
    if (is.null(usedFit)) 
        usedFit <- getKeywords()$usedFit
    if (!is.null(cutoff)) {
        usedFit <- intersect(usedFit, names(cutoff))
        cutoff <- cutoff[usedFit]
    }
    if (is.null(cutoff) && !is.null(nullObject)) {
        nullFit <- as.data.frame(nullObject[, usedFit])
        colnames(nullFit) <- usedFit
        temp <- rep(NA, length(usedFit))
        cutoff <- getCutoff(object = nullFit, alpha = alpha, revDirec = FALSE, usedFit = usedFit, 
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

# plotIndividualScatter: Plot each overlapping scatterplot showing the
# distribution of fit indices given the values of varying parameters

# \title{
# Plot an overlaying scatter plot visualizing the power of rejecting misspecified models
# }
# \description{
# Plot the fit indices value against the value of predictors. The plot will include the fit indices value of the alternative models, the fit indices value of the null model (if specified), and the fit indices cutoffs (if specified).  
# }
# \usage{
# plotIndividualScatter(altVec, nullVec=NULL, cutoff=NULL, x, main = NULL)
# }
# \arguments{
  # \item{altVec}{
	# The vector saving the fit index distribution when the hypothesized model is \code{FALSE}.
# }
  # \item{nullVec}{
	# The vector saving the fit index distribution when the hypothesized model is \code{TRUE}.
# }
  # \item{cutoff}{
	# A priori cutoff 
# }
# \item{x}{
	# The \code{data.frame} of the predictor values. The number of rows of the \code{x} argument should be equal to the number of rows in the \code{object}.
# }
  # \item{main}{
	# The title of the graph
# }
# }
# \value{
	# NONE. Only plot the fit indices distributions.
# }

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

# overlapHist: Plot overlapping histograms

# \title{
	# Plot overlapping histograms
# }
# \description{
	# Plot overlapping histograms
# }
# \usage{
# overlapHist(a, b, colors=c("red","blue","purple"), breaks=NULL, xlim=NULL, 
	# ylim=NULL, main=NULL, xlab=NULL, swap=FALSE)
# }
# \arguments{
  # \item{a}{
	# Data for the first histogram
# }
  # \item{b}{
	# Data for the second histogram
# }
  # \item{colors}{
	# Colors for the first histogram, the second histogram, and the overlappling areas.
# }
  # \item{breaks}{
	# How many breaks users used in each histogram (should not be used)
# }
  # \item{xlim}{
	# The range of x-axis
# }
  # \item{ylim}{
	# The range of y-axis
# }
  # \item{main}{
	# The title of the figure
# }
  # \item{xlab}{
	# The labels of x-axis
# }
  # \item{swap}{
	# Specify \code{TRUE} to plot \code{b} first and then \code{a}. The default is \code{FALSE} to plot \code{a} first and then \code{b}.
# }
# }
# \value{
	# None. This function will plot only.
# }
# \author{
    # Chris Miller provided this code on \url{http://chrisamiller.com/science/2010/07/20/transparent-overlapping-histograms-in-r/}. The code is modified by Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# }

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
