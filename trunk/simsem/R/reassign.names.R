reassign.names <- function(modelType, Name) {
	W <- get.keywords()
	result <- rep(NA, length(Name))
	keywords <- NULL
	if(modelType == "CFA") {
		keywords <- list(W$loading, W$error, W$latent.cor, W$intercept, W$factor.mean, c(W$VTE, W$VTD), c(W$VY, W$VX), c(W$VPS, W$VPH, W$VE), c(W$MY, W$MX))
	} else if(modelType == "Path") {
		keywords <- list(W$BE, W$RPS, W$AL, W$VPS, W$VE, W$ME)
	} else if(modelType == "Path.exo") {
		keywords <- list(W$BE, W$RPS, W$VPS, W$VE, W$AL, W$ME, W$GA, W$RPH, W$VPH, W$KA)
	} else if(modelType == "SEM") {
		keywords <- list(W$LY, W$RTE, W$VTE, W$VY, W$TY, W$MY, W$BE, W$RPS, W$VPS, W$VE, W$AL, W$ME)
	} else if(modelType == "SEM.exo") {
		keywords <- list(W$LY, W$RTE, W$VTE, W$VY, W$TY, W$MY, W$BE, W$RPS, W$VPS, W$VE, W$AL, W$ME, W$LX, W$RTD, W$VTD, W$VX, W$TX, W$MX, W$GA, W$RPH, W$VPH, W$KA, W$RTH)
	} else {
		stop("Cannot recognize the modelType name.")
	}
	position <- match.keyword(Name, keywords)
	if(sum(position == 0) > 0) stop(paste("Some matrices' names cannot be assigned in", modelType, "groups"))
	for(i in 1:length(Name)) {
		result[i] <- keywords[[position[i]]][1]
	}
	return(result)
}
