find.cutoff <- function(simResult, percentile, reverse = FALSE) {
	if(reverse) percentile <- 1 - percentile
	if(!is.matrix(simResult)) {
		Result <- simResult@Output
	} else {
		Result <- simResult
	}
	temp <- rep(NA, 8)
	temp <- apply(Result, 2, quantile, probs = percentile, na.rm = TRUE)
	temp[6] <- quantile(Result[,6], 1 - percentile, na.rm = TRUE)
	temp[7] <- quantile(Result[,7], 1 - percentile, na.rm = TRUE)
	return(temp)
}
