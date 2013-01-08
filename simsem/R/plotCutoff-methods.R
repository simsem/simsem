# plotCutoff: This function will plot sampling distributions of fit indices
# with vertical lines of cutoffs

setMethod("plotCutoff", signature(object = "data.frame"), definition = function(object, 
    cutoff = NULL, revDirec = FALSE, usedFit = NULL, vector1 = NULL, vector2 = NULL, 
    nameVector1 = NULL, nameVector2 = NULL, alpha = NULL, useContour = T, cutoff2 = NULL) {
	usedFit <- cleanUsedFit(usedFit)
    object <- as.data.frame(object[, usedFit])
    cutoff <- cutoff[usedFit]
    if (!is.null(cutoff2)) 
        cutoff2 <- cutoff2[usedFit]
    object <- as.data.frame(object[, !apply(object, 2, function(vec) all(is.na(vec)))])
    colnames(object) <- usedFit
    if (ncol(object) == 2) {
        obj <- par(mfrow = c(1, 2))
    } else if (ncol(object) == 3) {
        obj <- par(mfrow = c(1, 3))
    } else if (ncol(object) > 3) {
        obj <- par(mfrow = c(2, ceiling(ncol(object)/2)))
    } else if (ncol(object) == 1) {
        # Intentionally leaving as blank
    } else {
        stop("Some errors occur")
    }
    for (i in 1:ncol(object)) {
        val <- NULL
        if (!is.null(alpha)) {
            val <- 1 - alpha
            if (usedFit[i] %in% getKeywords()$reversedFit) 
                val <- alpha
        }
        if (is.null(vector1) & is.null(vector2)) {
            hist(object[, i], main = colnames(object)[i], breaks = 10, col = "yellow", 
                xlab = "Value")
            if (!is.null(cutoff)) 
                abline(v = cutoff[i], col = "red", lwd = 3)
            if (!is.null(cutoff2)) 
                abline(v = cutoff2[i], col = "red", lwd = 3)
        } else if (!is.null(vector1) & is.null(vector2)) {
            plotQtile(vector1, object[, i], xlab = nameVector1, ylab = "Value", main = colnames(object)[i], 
                qtile = val, df = 5)
        } else if (!is.null(vector1) & !is.null(vector2)) {
            plot3DQtile(vector1, vector2, object[, i], xlab = nameVector1, ylab = nameVector2, 
                zlab = "Value", main = colnames(object)[i], qtile = val, useContour = useContour, 
                df = 0)
        } else {
            stop("Something is wrong!")
        }
    }
    if (ncol(object) > 1) 
        par(obj)
})

setMethod("plotCutoff", signature(object = "SimResult"), definition = function(object, 
    alpha = NULL, revDirec = FALSE, usedFit = NULL, useContour = T) {
    object <- clean(object)
    cutoff <- NULL
    Data <- as.data.frame(object@fit)
    
    condition <- c(length(unique(object@pmMCAR)) > 1, length(unique(object@pmMAR)) > 
        1, length(unique(object@n)) > 1)
    condValue <- cbind(object@pmMCAR, object@pmMAR, object@n)
    colnames(condValue) <- c("Percent MCAR", "Percent MAR", "N")
    if (!is.null(alpha)) {
        if (revDirec) 
            alpha <- 1 - alpha
        if (all(!condition)) 
            cutoff <- getCutoff(Data, alpha)
    }
    if (sum(condition) == 0) {
        plotCutoff(Data, cutoff, revDirec, usedFit)
    } else if (sum(condition) == 1) {
        plotCutoff(Data, cutoff, revDirec, usedFit, vector1 = condValue[, condition], 
            nameVector1 = colnames(condValue)[condition], alpha = alpha)
    } else if (sum(condition) == 2) {
        condValue <- condValue[, condition]
        plotCutoff(Data, cutoff, revDirec, usedFit, vector1 = condValue[, 1], vector2 = condValue[, 
            2], nameVector1 = colnames(condValue)[1], nameVector2 = colnames(condValue)[2], 
            alpha = alpha, useContour = useContour)
    } else {
        stop("This function cannot plot when there more than two dimensions of varying parameters")
    }
    
}) 

# plot3DQtile: Build a persepctive plot or contour plot of a quantile of
# predicted values

# \title{
	# Build a persepctive plot or contour plot of a quantile of predicted values
# }
# \description{
	# Build a persepctive plot or contour plot of a quantile of predicted values
# }
# \usage{
# plot3DQtile(x, y, z, df=0, qtile=0.5, useContour=TRUE, xlab=NULL, 
	# ylab=NULL, zlab=NULL, main=NULL)
# }
# \arguments{
  # \item{x}{
	# The values of the first variable (e.g., a vector of sample size)
# }
  # \item{y}{
	# The values of the second variable (e.g., a vector of percent missing)
# }
  # \item{z}{
	# The values of the dependent variable
# }
  # \item{df}{
	# The degree of freedom in spline method
# }
  # \item{qtile}{
	# The quantile values used to plot a graph
# }
  # \item{useContour}{
	# If \code{TRUE}, use contour plot. If \code{FALSE}, use perspective plot.
# }
  # \item{xlab}{
	# The labels of x-axis
# }
  # \item{ylab}{
	# The labels of y-axis
# }
  # \item{zlab}{
	# The labels of z-axis
# }
  # \item{main}{
	# The title of the graph
# }
# }
# \value{
	# None. This function will plot only.
# }

plot3DQtile <- function(x, y, z, df = 0, qtile = 0.5, useContour = TRUE, xlab = NULL, 
    ylab = NULL, zlab = NULL, main = NULL) {
    library(quantreg)
    if (length(qtile) > 1) 
        stop("Please use only one quantile value at a time")
    xyz <- data.frame(x = x, y = y, z = z)
    xyz <- xyz[apply(is.na(xyz), 1, sum) == 0, ]
    mod <- NULL
    if (df == 0) {
        mod <- rq(z ~ x + y + x * y, data = xyz, tau = qtile)
    } else {
        library(splines)
        mod <- rq(z ~ ns(x, df) + ns(y, df) + ns(x, df) * ns(y, df), data = xyz, 
            tau = qtile)
    }
    xseq <- seq(min(x), max(x), length = 20)
    yseq <- seq(min(y), max(y), length = 20)
    f <- function(x, y) {
        r <- mod$coefficients[1] + mod$coefficients[2] * x + mod$coefficients[3] * 
            y + mod$coefficients[3] * x * y
    }
    zpred <- outer(xseq, yseq, f)
    if (useContour) {
        contour(xseq, yseq, zpred, xlab = xlab, ylab = ylab, main = main)
    } else {
        persp(xseq, yseq, zpred, theta = 30, phi = 30, expand = 0.5, col = "lightblue", 
            ltheta = 120, shade = 0.75, ticktype = "detailed", xlab = xlab, ylab = ylab, 
            main = main, zlab = zlab)
    }
} 

# plotQtile: Build a scatterplot with overlaying line of quantiles of predicted
# values

# \title{
	# Build a scatterplot with overlaying line of quantiles of predicted values
# }
# \description{
	# Build a scatterplot with overlaying line of quantiles of predicted values
# }
# \usage{
# plotQtile(x, y, df=0, qtile=NULL, ...)
# }
# \arguments{
  # \item{x}{
	# The values of the independent variable (e.g., a vector of sample size)
# }
  # \item{y}{
	# The values of the dependent variable
# }
  # \item{df}{
	# The degree of freedom in spline method
# }
  # \item{qtile}{
	# The quantile values used to plot a graph
# }
  # \item{\dots}{
	# Other arguments in the \code{plot} command
# }
# }
# \value{
	# None. This function will plot only.
# }

plotQtile <- function(x, y, df = 0, qtile = NULL, ...) {
    library(quantreg)
    xy <- data.frame(x = x, y = y)
    plot(x, y, ...)
    if (!is.null(qtile)) {
        mod <- NULL
        if (df == 0) {
            mod <- rq(y ~ x, tau = qtile)
        } else {
            library(splines)
            mod <- rq(y ~ ns(x, df), tau = qtile)
        }
        xseq <- seq(min(x), max(x), length = nrow(xy))
        pred <- predict(mod, data.frame(x = xseq), interval = "none", level = 0.95)
        pred <- as.matrix(pred)
        for (i in 1:ncol(pred)) {
            lines(xseq, pred[, i], col = "red")
        }
    }
} 
