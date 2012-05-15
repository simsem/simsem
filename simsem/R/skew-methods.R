# skew: Find an skewness of an object.

setMethod("skew", signature(object = "vector"), function(object, population = FALSE) {
    if (population) {
        return(centralMoment(object, 3)/(centralMoment(object, 2)^(3/2)))
    } else {
        est <- kStat(object, 3)/(kStat(object, 2)^(3/2))
        se <- sqrt(6/length(object))
        z <- est/se
        p <- (1 - pnorm(abs(z))) * 2
        return(c(`skew (g1)` = est, se = se, z = z, p = p))
    }
})

setMethod("skew", signature(object = "VirtualDist"), function(object, reverse = FALSE, bin = 1e+05) {
    distName <- class(object)
    distName <- tolower(gsub("Sim", "", distName))
    funmin <- list(get(paste("q", distName, sep = "")), 1e-06)
    funmax <- list(get(paste("q", distName, sep = "")), 0.999999)
    indivAttr <- slotNames(object)
    for (j in 1:length(indivAttr)) {
        funmin[[j + 2]] <- call("=", indivAttr[[j]], slot(object, indivAttr[[j]]))
        funmax[[j + 2]] <- funmin[[j + 2]]
    }
    xlim <- rep(0, 0)
    xlim[1] <- eval(as.call(funmin))
    xlim[2] <- eval(as.call(funmax))
    xrange <- seq(xlim[1], xlim[2], length.out = bin)
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
    return(centralMoment(xrange, 3, yrange)/(centralMoment(xrange, 2, yrange)^(3/2)))
}) 
