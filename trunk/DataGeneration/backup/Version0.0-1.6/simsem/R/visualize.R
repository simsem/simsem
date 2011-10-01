visualize <- function(simResult, percentile=NULL, reverse = FALSE) {
	cutoff <- rep(NA, 7)
	if(!is.null(percentile)) {
		if(reverse) percentile <- 1 - percentile
		cutoff <- find.cutoff(simResult, percentile)[-2]
	}
	if(!is.matrix(simResult)) {
		Result <- simResult@Output[,-2]
	} else {
		Result <- simResult[,-2]
	}
	k <- 0
	for(i in 1:ncol(Result)) {
		if(!is.na.vector(Result[,i])) k = k + 1
	}
	obj <- par(mfrow = c(2, ceiling(k/2)))
	Kept.Fit <- c("Chi-square", "AIC", "BIC", "RMSEA", "CFI", "TLI", "SRMR")
	for(i in 1:ncol(Result)) {
		if(!is.na.vector(Result[,i])) { k = k + 1
			hist(Result[,i], main = Kept.Fit[i], breaks = 10, col="yellow", xlab = "value")
			if(!is.null(percentile)) abline(v = cutoff[i], col="red")
		}
	}
	par(obj)
}
