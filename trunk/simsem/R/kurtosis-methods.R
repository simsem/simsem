# kurtosis
# Methods -- simsem package
# Find an excessive kurtosis of an object.
# Generic Function: run(object, ...)
# Argument:
#	x:  an object in simsem that users wish to run
# 	... : Other arguments, such as data
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: February 26, 2012

setMethod("kurtosis", signature(object = "vector"), function(object, population=FALSE) {
	if(population) {
		return((centralMoment(object, 4)/(centralMoment(object, 2)^2)) - 3)
	} else {
		est <- kStat(object, 4)/(kStat(object, 2)^(2))
		se <- sqrt(24/length(object))
		z <- est/se
		p <- (1 - pnorm(abs(z)))*2
		return(c("Excess Kur (g2)"=est, se=se, z=z, p=p))
	}
}
)

setMethod("kurtosis", signature(object = "VirtualDist"), function(object, reverse=FALSE, bin=100000) {
		distName <- class(object)
		distName <- tolower(gsub("Sim", "", distName))
		funmin <- list(get(paste("q", distName, sep="")), 0.000001)
		funmax <- list(get(paste("q", distName, sep="")), 0.999999)
		indivAttr <- slotNames(object)
		for(j in 1:length(indivAttr)) {
			funmin[[j+2]] <- call("=", indivAttr[[j]], slot(object, indivAttr[[j]]))
			funmax[[j+2]] <- funmin[[j+2]]
		}
		xlim <- rep(0, 0)
		xlim[1] <- eval(as.call(funmin))
		xlim[2] <- eval(as.call(funmax))
		xrange <- seq(xlim[1], xlim[2], length.out=bin)
		fun <- list(get(paste("d", distName, sep="")))
		fun[[2]] <- xrange
		for(j in 1:length(indivAttr)) {
			fun[[j+2]] <- call("=", indivAttr[[j]], slot(object, indivAttr[[j]]))
		}
		yrange <- eval(as.call(fun))
		if(reverse) {
			wMeanOld <- sum(xrange * yrange)/sum(yrange)
			disLeftOld <- wMeanOld - min(xrange)
			disRightOld <- max(xrange) - wMeanOld
			yrange <- rev(yrange)
			wMeanNew <- sum(xrange * yrange)/sum(yrange)
			xrange <- seq(wMeanNew - disRightOld, wMeanNew + disLeftOld, length.out=length(xrange))
		}
		return((centralMoment(xrange, 4, yrange)/(centralMoment(xrange, 2, yrange)^2)) - 3)
	}
)	