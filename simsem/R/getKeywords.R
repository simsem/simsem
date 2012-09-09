# getKeywords: List of all keywords used in the simsem package

# \title{
	# List of all keywords used in the \code{simsem} package
# }
# \description{
	# List of all keywords used in the \code{simsem} package
# }
# \usage{
# getKeywords()
# }
# \value{
	# A list of all keywords used in this package
	# \itemize{
		# \item \code{usedFit} Fit indices used as the default for providing output
		# \item \code{usedFitPop} Population fit indices used as the default for providing input
		# \item \code{optMin} The method picking the minimum value of misfit across misspecification sets
		# \item \code{optMax} The method picking the maximum value of misfit across misspecification sets
		# \item \code{optNone} Not using the optimization method
	# }
# }

getKeywords <- function() {
    
    usedFit <- c("Chi", "AIC", "BIC", "RMSEA", "CFI", "TLI", "SRMR")
    usedFitPop <- c("f0", "rmsea", "srmr")
    optMin <- c("min", "minimum", "lower")
    optMax <- c("max", "maximum", "upper")
    optNone <- c("none", "null")
    
    result <- list(usedFit = usedFit, usedFitPop = usedFitPop, optMin = optMin, optMax = optMax, 
        optNone = optNone)
    return(result)
} 
