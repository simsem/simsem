# plotDist: This function will plot a distribution

setMethod("plotDist", signature = "SimDataDist", definition = function(object, xlim = NULL, ylim = NULL, r = 0, var = NULL) {
    if (isNullObject(object)) 
        stop("The data distribution object is not specified yet.")
    if (!is.null(var)) {
        if (!is.vector(var)) 
            stop("Please specify a vector (no more than two elements) of variables")
        if (length(var) > 2) 
            stop("The length of the variables you wish to plot is larger than two")
        object <- extract(object, var)
    }
    if (object@p == 1) {
        plotDist(object@dist[[1]], reverse = object@reverse[1], xlim = xlim)
    } else if (object@p == 2) {
        library(copula)
        CopNorm <- ellipCopula(family = "normal", dim = object@p, dispstr = "un", param = r)
        distName <- sapply(object@dist, class)
        distName <- tolower(gsub("Sim", "", distName))
        attribute <- list()
        lim <- list()
        for (i in 1:length(object@dist)) {
            temp <- list()
            indivAttr <- slotNames(object@dist[[i]])
            for (j in 1:length(indivAttr)) {
                temp[[j]] <- call("=", indivAttr[[j]], slot(object@dist[[i]], indivAttr[[j]]))
            }
            attribute[[i]] <- temp
        }
        Mvdc <- mvdc(CopNorm, distName, attribute)
        ######################### xlim
        if (is.null(xlim)) {
            xfunmin <- list(get(paste("q", distName[1], sep = "")), 0.005)
            xfunmax <- list(get(paste("q", distName[1], sep = "")), 0.995)
            xAttr <- slotNames(object@dist[[1]])
            for (j in 1:length(xAttr)) {
                xfunmin[[j + 2]] <- call("=", xAttr[[j]], slot(object@dist[[1]], xAttr[[j]]))
                xfunmax[[j + 2]] <- xfunmin[[j + 2]]
            }
            xlim <- rep(0, 0)
            xlim[1] <- eval(as.call(xfunmin))
            xlim[2] <- eval(as.call(xfunmax))
        }
        ######################### ylim
        if (is.null(ylim)) {
            yfunmin <- list(get(paste("q", distName[2], sep = "")), 0.005)
            yfunmax <- list(get(paste("q", distName[2], sep = "")), 0.995)
            yAttr <- slotNames(object@dist[[2]])
            for (j in 1:length(yAttr)) {
                yfunmin[[j + 2]] <- call("=", yAttr[[j]], slot(object@dist[[2]], yAttr[[j]]))
                yfunmax[[j + 2]] <- yfunmin[[j + 2]]
            }
            ylim <- rep(0, 0)
            ylim[1] <- eval(as.call(yfunmin))
            ylim[2] <- eval(as.call(yfunmax))
        }
        xis <- seq(xlim[1], xlim[2], length = 51)
        yis <- seq(ylim[1], ylim[2], length = 51)
        grids <- as.matrix(expand.grid(xis, yis))
        zmat <- matrix(dmvdc(Mvdc, grids), 51, 51)
        if (object@reverse[1]) {
            zmat <- zmat[nrow(zmat):1, ]
            den <- apply(zmat, 1, sum)
            wMeanOld <- sum(xis * den)/sum(den)
            disLeftOld <- wMeanOld - min(xis)
            disRightOld <- max(xis) - wMeanOld
            den <- rev(den)
            wMeanNew <- sum(xis * den)/sum(den)
            xis <- seq(wMeanNew - disRightOld, wMeanNew + disLeftOld, length.out = length(xis))
        }
        if (object@reverse[2]) {
            zmat <- zmat[, ncol(zmat):1]
            den <- apply(zmat, 2, sum)
            wMeanOld <- sum(yis * den)/sum(den)
            disLeftOld <- wMeanOld - min(yis)
            disRightOld <- max(yis) - wMeanOld
            den <- rev(den)
            wMeanNew <- sum(yis * den)/sum(den)
            yis <- seq(wMeanNew - disRightOld, wMeanNew + disLeftOld, length.out = length(yis))
        }
        contour(xis, yis, zmat, xlab = "Varible 1", ylab = "Variable 2")
        val <- list(x = xis, y = yis, z = zmat)
        invisible(val)
    }
})

setMethod("plotDist", signature = "VirtualDist", definition = function(object, xlim = NULL, reverse = FALSE) {
    distName <- class(object)
    distName <- tolower(gsub("Sim", "", distName))
    if (is.null(xlim)) {
        funmin <- list(get(paste("q", distName, sep = "")), 0.005)
        funmax <- list(get(paste("q", distName, sep = "")), 0.995)
        indivAttr <- slotNames(object)
        for (j in 1:length(indivAttr)) {
            funmin[[j + 2]] <- call("=", indivAttr[[j]], slot(object, indivAttr[[j]]))
            funmax[[j + 2]] <- funmin[[j + 2]]
        }
        xlim <- rep(0, 0)
        xlim[1] <- eval(as.call(funmin))
        xlim[2] <- eval(as.call(funmax))
    }
    xrange <- seq(xlim[1], xlim[2], length.out = 200)
    fun <- list(get(paste("d", distName, sep = "")))
    fun[[2]] <- xrange
    for (j in 1:length(indivAttr)) {
        fun[[j + 2]] <- call("=", indivAttr[[j]], slot(object, indivAttr[[j]]))
    }
    yrange <- eval(as.call(fun))
    if (reverse) {
        wMeanOld <- sum(xrange * yrange)/sum(yrange)
        disLeftOld <- wMeanOld - min(xrange)
        disRightOld <- max(xrange) - wMeanOld
        yrange <- rev(yrange)
        wMeanNew <- sum(xrange * yrange)/sum(yrange)
        xrange <- seq(wMeanNew - disRightOld, wMeanNew + disLeftOld, length.out = length(xrange))
    }
    plot(xrange, yrange, type = "n", xlab = "value", ylab = "density")
    lines(xrange, yrange)
}) 
