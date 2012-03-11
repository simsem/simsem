# reassign.names
# Function -- simsem package
# Match the rownames of the equality constraint, check whether it match any model matrices, and substitute with an appropriate matrix name.
# Function: reassign.names(modelType, Name)
# Argument:
# 	modelType:	Type of the analysis model
#	Name: 	Row of matrices name in the equality constraint
# Return: 	Row of matrices name with an appropriate name of model matrices
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: March 10, 2012

reassign.names <- function(modelType, Name) {
	W <- get.keywords()
	result <- rep(NA, length(Name))
	keywords <- NULL
	if(modelType == "CFA") {
		keywords <- list(W$loading, W$errorCor, W$facCor, W$errorVar, W$indicatorVar, W$intercept, W$facMean, W$indicatorMean, W$facVar, W$errorCov, W$facCov) # 11 total
	} else if(modelType == "Path") {
		keywords <- list(W$BE, W$RPS, W$VPS, W$VE, W$AL, W$ME, W$PS) # Length = 7
	} else if(modelType == "Path.exo") {
		keywords <- list(W$BE, W$RPS, W$VPS, W$VE, W$AL, W$ME, W$PS, W$GA, W$RPH, W$VPH, W$KA, W$PH) # Length = 12
	} else if(modelType == "SEM") {
		keywords <- list(W$LY, W$RTE, W$VTE, W$VY, W$TY, W$MY, W$BE, W$RPS, W$VPS, W$VE, W$AL, W$ME, W$TE, W$PS) #Length = 14
	} else if(modelType == "SEM.exo") {
		keywords <- list(W$LY, W$RTE, W$VTE, W$VY, W$TY, W$MY, W$BE, W$RPS, W$VPS, W$VE, W$AL, W$ME, W$TE, W$PS, W$LX, W$RTD, W$VTD, W$VX, W$TX, W$MX, W$GA, W$RPH, W$VPH, W$KA, W$RTH, W$TD, W$PH, W$TH) #Length = 28
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
