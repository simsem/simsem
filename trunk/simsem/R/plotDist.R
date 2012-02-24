# plotCutoff
# Methods -- simsem package
# This function will plot sampling distributions of fit indices with vertical lines of cutoffs
# Generic Function: plotCutoff(object, ...)
# Argument:
#	object: 	The object (SimResult.c or data.frame.c) that contains values of fit indices in each distribution.
#	...:		Other argments such as cutoff values
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 9, 2011

setMethod("plotDist", signature="SimDataDist", definition=function(object, r=0, var=NULL, xlim=NULL, ylim=NULL) {
	if(is.null.object(object)) stop("The data distribution object is not specified yet.")
	if(!is.null(var)) {
		if(!is.vector(var)) stop("Please specify a vector (no more than two elements) of variables")
		if(length(var) > 2) stop("The length of the variables you wish to plot is larger than two")
		object <- extract(object, var)
	}
	if(object@p == 1) {
		plotDist(object@dist[[1]])
	} else if (object@p == 2) {
		library(copula)
		CopNorm <- ellipCopula(family = "normal", dim = object@p, dispstr = "un", param = r)
		distName <- sapply(object@dist, class)
		distName <- tolower(gsub("Sim", "", distName))
		attribute <- list()
		lim <- list()
		for(i in 1:length(object@dist)) {
			temp <- list()
			indivAttr <- slotNames(object@dist[[i]])
			for(j in 1:length(indivAttr)) {
				temp[[j]] <- call("=", indivAttr[[j]], slot(object@dist[[i]], indivAttr[[j]]))
			}
			attribute[[i]] <- temp
		}
		Mvdc <- mvdc(CopNorm, distName, attribute)
		######################### xlim 
		if(is.null(xlim)) {
			xfunmin <- list(get(paste("q", distName[1], sep="")), 0.005)
			xfunmax <- list(get(paste("q", distName[1], sep="")), 0.995)
			xAttr <- slotNames(object@dist[[1]])
			for(j in 1:length(xAttr)) {
				xfunmin[[j+2]] <- call("=", xAttr[[j]], slot(object@dist[[1]], xAttr[[j]]))
				xfunmax[[j+2]] <- xfunmin[[j+2]]
			}
			xlim <- rep(0, 0)
			xlim[1] <- eval(as.call(xfunmin))
			xlim[2] <- eval(as.call(xfunmax))
		}
		######################### ylim
		if(is.null(ylim)) {
			yfunmin <- list(get(paste("q", distName[2], sep="")), 0.005)
			yfunmax <- list(get(paste("q", distName[2], sep="")), 0.995)
			yAttr <- slotNames(object@dist[[2]])
			for(j in 1:length(yAttr)) {
				yfunmin[[j+2]] <- call("=", yAttr[[j]], slot(object@dist[[2]], yAttr[[j]]))
				yfunmax[[j+2]] <- yfunmin[[j+2]]
			}
			ylim <- rep(0, 0)
			ylim[1] <- eval(as.call(yfunmin))
			ylim[2] <- eval(as.call(yfunmax))
		}
		######################### making contour
		contour(Mvdc, dmvdc, xlim = xlim, ylim = ylim, xlab="Varible 1", ylab="Variable 2")
	}
}
)


setMethod("plotDist", signature="VirtualDist", definition=function(object, xlim=NULL) {
	distName <- class(object)
	distName <- tolower(gsub("Sim", "", distName))
	if(is.null(xlim)) {
		funmin <- list(get(paste("q", distName, sep="")), 0.005)
		funmax <- list(get(paste("q", distName, sep="")), 0.995)
		indivAttr <- slotNames(object)
		for(j in 1:length(indivAttr)) {
			funmin[[j+2]] <- call("=", indivAttr[[j]], slot(object, indivAttr[[j]]))
			funmax[[j+2]] <- funmin[[j+2]]
		}
		xlim <- rep(0, 0)
		xlim[1] <- eval(as.call(funmin))
		xlim[2] <- eval(as.call(funmax))
	}
	xrange <- seq(xlim[1], xlim[2], length.out=200)
	fun <- list(get(paste("d", distName, sep="")))
	fun[[2]] <- xrange
	for(j in 1:length(indivAttr)) {
		fun[[j+2]] <- call("=", indivAttr[[j]], slot(object, indivAttr[[j]]))
	}
	yrange <- eval(as.call(fun))
	plot(xrange, yrange, type="n", xlab="value", ylab="density")
	lines(xrange, yrange)
}
)