# plotQtile
# Function -- simsem package
# Build a scatterplot with overlaying line of quantiles of predicted values
# Argument:
#	x: 	X values
#	y:	Y values
#	df:		Degree of freedom for the spline function
#	qtile:	Quantile values to be plotted
# 	...:	Other specification in the plot function
# Author: 	Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)

plotQtile <- function(x, y, df=0, qtile=0.5, ...) {
	library(quantreg)
	xy <- data.frame(x=x, y=y)
	plot(x, y, ...)
	mod <- NULL
	if(df == 0) {
		mod <- rq(y ~ x, tau=qtile)
	} else {
		library(splines)
		mod <- rq(y ~ ns(x, df), tau=qtile)
	}
    xseq <- seq(min(x), max(x), length = nrow(xy))
    pred <- predict(mod, data.frame(x = xseq), interval = "none", level = 0.95)
	pred <- as.matrix(pred)
	for(i in 1:ncol(pred)) {
	lines(xseq, pred[,i], col="red")
	}
}
