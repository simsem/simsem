findFitIndicesOpenMx <- function(indiv.result) {
	temp.result <- summary(indiv.result)
	Chi <- temp.result$Chi
	p <- temp.result$p
	AIC <- temp.result$AIC.Mx
	BIC <- temp.result$BIC.Mx
	degree.freedom <- temp.result$degreesOfFreedom
	ni <- dim(temp.result$data$Model.data$cov)[1]
	no <- temp.result$numObs
	temp <- ((Chi / degree.freedom) - 1) / (no - 1)
	if(temp < 0) temp <- 0
	RMSEA <- sqrt(temp)
	result <- list(Chi = Chi, p.Chi = p, AIC = AIC, BIC = BIC, RMSEA = RMSEA)
	return(result)
}
