reassign.names <- function(Tag, Name) {
	W <- get.keywords()
	result <- rep(NA, length(Name))
	keywords <- NULL
	if(Tag == "CFA") {
		keywords <- list(W$loading, W$error, W$latent.cor, W$intercept, W$factor.mean, c(W$VTE, W$VTD), c(W$VY, W$VX), c(W$VPS, W$VPH, W$VE), c(W$MY, W$MX))
	} else if(Tag == "Path") {
		keywords <- list(W$BE, W$PS, W$AL, W$VPS, W$VE, W$ME)
	} else if(Tag == "Path.exo") {
		keywords <- list(W$BE, W$PS, W$VPS, W$VE, W$AL, W$ME, W$GA, W$PH, W$VPH, W$KA)
	} else if(Tag == "SEM") {
		keywords <- list(W$LY, W$TE, W$VTE, W$VY, W$TY, W$MY, W$BE, W$PS, W$VPS, W$VE, W$AL, W$ME)
	} else if(Tag == "SEM.exo") {
		keywords <- list(W$LY, W$TE, W$VTE, W$VY, W$TY, W$MY, W$BE, W$PS, W$VPS, W$VE, W$AL, W$ME, W$LX, W$TD, W$VTD, W$VX, W$TX, W$MX, W$GA, W$PH, W$VPH, W$KA, W$TH)
	} else {
		stop("Cannot recognize the Tag name.")
	}
	position <- match.keyword(Name, keywords)
	if(sum(position == 0) > 0) stop(paste("Some matrices' names cannot be assigned in", Tag, "groups"))
	for(i in 1:length(Name)) {
		result[i] <- keywords[[position[i]]][1]
	}
	return(result)
}
