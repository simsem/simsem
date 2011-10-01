find.power <- function(Result.alternative, cutoff, reverse = FALSE) {
	#browser()
	if(!is.matrix(Result.alternative)) {
		output <- Result.alternative@Output
	} else {
		output <- Result.alternative
	}
	Chi = pvalue(output[, 1], cutoff["Chi"], reverse)
	p = pvalue(output[, 2], cutoff["p"], reverse)
	AIC = pvalue(output[,3], cutoff["AIC"], reverse)
	BIC = pvalue(output[,4], cutoff["BIC"], reverse)
	RMSEA = pvalue(output[, 5], cutoff["RMSEA"], reverse)
	CFI = 1 - pvalue(output[, 6], cutoff["CFI"], reverse)
	TLI = 1 - pvalue(output[, 7], cutoff["TLI"], reverse)
	SRMR = pvalue(output[, 8], cutoff["SRMR"], reverse)
	result <- c(Chi, p, AIC, BIC, RMSEA, CFI, TLI, SRMR)
	names(result) <- names(cutoff)
	return(result)
}
