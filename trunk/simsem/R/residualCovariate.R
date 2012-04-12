# residualCovariate
# function -- simsem package
# Residual centered all target indicators by covariates
# Argument:
#	data: target data
# 	targetVar: Varible names or the position of indicators that users wish to be residual centered
#	covVar: Covariate names using for residual centering (as independent variables) onto target variables
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: April 12, 2012

residualCovariate <- function(data, targetVar, covVar) {
	x <- as.list(match.call())
	cov <- eval(x$covVar)
	target <- eval(x$targetVar)
	if(all(is.numeric(cov))) cov <- colnames(data)[cov]
	if(all(is.numeric(target))) target <- colnames(data)[target]
	express <- paste("cbind(", paste(target, collapse=", "), ") ~ ", paste(cov, collapse=" + "), sep="")
	data[,target] <- lm(express, data=data)$residuals
	return(data)
}