# plotDist: This function will plot a distribution

plotDist <- function(object, xlim = NULL, ylim = NULL, r = 0, var = NULL, contour = TRUE) {
    if (!is.null(var)) {
        if (!is.vector(var)) 
            stop("Please specify a vector (no more than two elements) of variables")
        if (length(var) > 2) 
            stop("The length of the variables you wish to plot is larger than two")
        object <- extractSimDataDist(object, var)
    }
    if (object@p == 1) {
        plotDist1D(object@margins[1], object@paramMargins[[1]], reverse = object@reverse[1], xlim = xlim)
    } else if (object@p == 2) {
		plotDist2D(object@margins[1:2], object@paramMargins[1:2], reverse = object@reverse[1:2], xlim = xlim, ylim = ylim, r = r, contour=contour)
    } else {
		stop("The dimension cannot be greater than 2.")
	}
}

plotDist1D <- function(distName, param, xlim = NULL, reverse = FALSE) {
    if (is.null(xlim)) {
        funmin <- c(list(get(paste("q", distName, sep = "")), 0.005), param)
        funmax <- c(list(get(paste("q", distName, sep = "")), 0.995), param)
        xlim <- rep(0, 0)
        xlim[1] <- eval(as.call(funmin))
        xlim[2] <- eval(as.call(funmax))
    }
    xrange <- seq(xlim[1], xlim[2], length.out = 200)
    fun <- c(list(get(paste("d", distName, sep = "")), xrange), param)
    yrange <- eval(as.call(fun))
    if (reverse) {
        wMeanOld <- sum(xrange * yrange)/sum(yrange)
        disLeftOld <- wMeanOld - min(xrange)
        disRightOld <- max(xrange) - wMeanOld
        yrange <- rev(yrange)
        wMeanNew <- sum(xrange * yrange)/sum(yrange)
        xrange <- seq(wMeanNew - disRightOld, wMeanNew + disLeftOld, length.out = length(xrange))
    }
    plot(xrange, yrange, type = "n", xlab = "Value", ylab = "Density")
    lines(xrange, yrange)
}

plotDist2D <- function(distName, param, xlim = NULL, ylim = NULL, r = 0, reverse=rep(FALSE,2), contour=TRUE) {
	library(copula)
	CopNorm <- ellipCopula(family = "normal", dim = 2, dispstr = "un", param = r)
	Mvdc <- mvdc(CopNorm, distName, param)
	######################### xlim
	if (is.null(xlim)) {
		xfunmin <- c(list(get(paste("q", distName[1], sep = "")), 0.005), param[[1]])
		xfunmax <- c(list(get(paste("q", distName[1], sep = "")), 0.995), param[[1]])
		xlim <- rep(0, 0)
		xlim[1] <- eval(as.call(xfunmin))
		xlim[2] <- eval(as.call(xfunmax))
	}
	######################### ylim
	if (is.null(ylim)) {
		yfunmin <- c(list(get(paste("q", distName[2], sep = "")), 0.005), param[[2]])
		yfunmax <- c(list(get(paste("q", distName[2], sep = "")), 0.995), param[[2]])
		ylim <- rep(0, 0)
		ylim[1] <- eval(as.call(yfunmin))
		ylim[2] <- eval(as.call(yfunmax))
	}
	xis <- seq(xlim[1], xlim[2], length = 51)
	yis <- seq(ylim[1], ylim[2], length = 51)
	grids <- as.matrix(expand.grid(xis, yis))
	zmat <- matrix(dMvdc(grids, Mvdc), 51, 51)
	if (reverse[1]) {
		zmat <- zmat[nrow(zmat):1, ]
		den <- apply(zmat, 1, sum)
		wMeanOld <- sum(xis * den)/sum(den)
		disLeftOld <- wMeanOld - min(xis)
		disRightOld <- max(xis) - wMeanOld
		den <- rev(den)
		wMeanNew <- sum(xis * den)/sum(den)
		xis <- seq(wMeanNew - disRightOld, wMeanNew + disLeftOld, length.out = length(xis))
	}
	if (reverse[2]) {
		zmat <- zmat[, ncol(zmat):1]
		den <- apply(zmat, 2, sum)
		wMeanOld <- sum(yis * den)/sum(den)
		disLeftOld <- wMeanOld - min(yis)
		disRightOld <- max(yis) - wMeanOld
		den <- rev(den)
		wMeanNew <- sum(yis * den)/sum(den)
		yis <- seq(wMeanNew - disRightOld, wMeanNew + disLeftOld, length.out = length(yis))
	}
	if(contour) {
		contour(xis, yis, zmat, xlab = "Varible 1", ylab = "Variable 2")
	} else {
		persp(xis, yis, zmat, xlab = "Varible 1", ylab = "Variable 2", zlab = "Density")
	}
	val <- list(x = xis, y = yis, z = zmat)
	invisible(val)
}
