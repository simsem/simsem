# summaryShort
# Methods -- simsem package
# Provide short summary if it is available. Otherwise, it is an alias for summary.
# Generic Function: summaryShort(object, ...)
# Argument:
#	object: The target object that is used to summarize
# 	... : Other arguments (None is specified currently)
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: November 15, 2011

setMethod("summaryShort", signature="ANY", definition = function(object) {
		summary(object)
	}
)
#Arguments: 
#	object:	Anything
#Description: Alias for summary function.
#Return: 	NONE. Results will print on screen only.

setMethod("summaryShort", signature="SimMatrix", definition = function(object) {
		Data <- object@free
		Labels <- object@param
		Labels[!is.na(Data)] <- as.character(Data[!is.na(Data)])
		Labels[is.na(Data)] <- paste("NA:", Labels[is.na(Data)], sep="")
		print(Labels)
	}
)
#Arguments: 
#	object:	SimMatrix.c that users wish to summarize shortly
#Description: This function will print all fixed values and free values into the same matrix.
#Return: 	NONE. Results will print on screen only.
#Examples:
#u89 <- simUnif(0.8, 0.9)
#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#LX <- simMatrix(loading, "u89")
#summaryShort(LX)

setMethod("summaryShort", signature="SimVector", definition = function(object) {
		Data <- object@free
		Labels <- object@param
		Labels[!is.na(Data)] <- as.character(Data[!is.na(Data)])
		Labels[is.na(Data)] <- paste("NA:", Labels[is.na(Data)], sep="")
		print(Labels)
	}
)
#Arguments: 
#	object:	SimVector.c that users wish to summarize shortly
#Description: This function will print all fixed values and free values into the same matrix.
#Return: 	NONE. Results will print on screen only.

setMethod("summaryShort", signature="vector", definition=function(object) {
		print(object)
	}
)
#Arguments: 
#	object:	vector.c that users wish to summarize shortly
#Description: This function will print the object. (Different from summary function from main packages in R).
#Return: 	NONE. Results will print on screen only.

setMethod("summaryShort", signature="matrix", definition=function(object) {
		print(object)
	}
)
#Arguments: 
#	object:	matrix.c that users wish to summarize shortly
#Description: This function will print the object. (Different from summary function from main packages in R).
#Return: 	NONE. Results will print on screen only.

################################################################################
# Distribution object: Provide a summary of each distribution object

setMethod("summaryShort",
    signature(object = "SimNorm"),
    function (object)
    {
		lab <- makeLabels(object)
		cat(paste("Random Normal Distribution Object: ", lab, ".\n", sep=""))
    }
)
# Normal Distribution
# Argument: mean = population mean, sd = standard deviation

setMethod("summaryShort",
    signature(object = "SimUnif"),
    function (object) 
    {
		lab <- makeLabels(object)
		cat(paste("Random Uniform Distribution Object: ", lab, ".\n", sep=""))
    }
)
# Uniform Distribution
# Argument: min = lower bound, max = upper bound

setMethod("summaryShort",
    signature(object = "SimBeta"),
    function (object)
    {
		lab <- makeLabels(object)
		cat(paste("Random Beta Distribution Object: ", lab, ".\n", sep=""))
    }
)
# Beta Distribution
# Attributes: shape1, shape2 = positive numbers of beta distributions, ncp = non-centrality parameter (shape1, shape2 > 0)

setMethod("summaryShort",
    signature(object = "SimBinom"),
    function (object)
    {
		lab <- makeLabels(object)
		cat(paste("Random Binomial Distribution Object: ", lab, ".\n", sep=""))
    }
)
# Binomial Distribution
# Attributes: size = Number of trials (zero or more), prob = probability of success on each trial (0 to 1)

setMethod("summaryShort",
    signature(object = "SimCauchy"),
    function (object)
    {
		lab <- makeLabels(object)
		cat(paste("Random Cauchy Distribution Object: ", lab, ".\n", sep=""))
    }
)
# Cauchy Distribution
# Attributes: location = location parameter, scale = scale parameter (> 0)

setMethod("summaryShort",
    signature(object = "SimChisq"),
    function (object)
    {
		lab <- makeLabels(object)
		cat(paste("Random Chi-squared Distribution Object: ", lab, ".\n", sep=""))
    }
)
# Chi-squared Distribution
# Attributes: df = degrees of freedom (non-negative), ncp = non-centrality parameter (non-negative)

setMethod("summaryShort",
    signature(object = "SimExp"),
    function (object)
    {
		lab <- makeLabels(object)
		cat(paste("Random Exponential Distribution Object: ", lab, ".\n", sep=""))
    }
)
# Exponential Distribution
# Attributes: rate = rate parameter

setMethod("summaryShort",
    signature(object = "SimF"),
    function (object)
    {
		lab <- makeLabels(object)
		cat(paste("Random F Distribution Object: ", lab, ".\n", sep=""))
    }
)
# F-distribution
# Attributes: df1, df2 = degrees of freedom (>0), ncp = non-centrality parameter (>=0)

setMethod("summaryShort",
    signature(object = "SimGamma"),
    function (object)
    {
		lab <- makeLabels(object)
		cat(paste("Random Gamma Distribution Object: ", lab, ".\n", sep=""))
    }
)
# Gamma Distribution
# Attributes: shape = Shape parameter, scale = Scale parameter

setMethod("summaryShort",
    signature(object = "SimGeom"),
    function (object)
    {
		lab <- makeLabels(object)
		cat(paste("Random Geometric Distribution Object: ", lab, ".\n", sep=""))
    }
)
# Geometric Distribution
# Attributes: prob = probability of successes

setMethod("summaryShort",
    signature(object = "SimHyper"),
    function (object)
    {
		lab <- makeLabels(object)
		cat(paste("Random Hypergeometric Distribution Object: ", lab, ".\n", sep=""))
    }
)
# Hypergeometric Distribution
# Attributes: m = The number of successes, n = The number of failures, k =  The number of drawns (All are integers)

setMethod("summaryShort",
    signature(object = "SimLnorm"),
    function (object)
    {
		lab <- makeLabels(object)
		cat(paste("Random Log Normal Distribution Object: ", lab, ".\n", sep=""))
    }
)
# Log Normal Distribution
# Attributes: meanlog = mean of the distribution in log scale, sdlog = standard deviation of the distribution in log scale (sdlog > 0)

setMethod("summaryShort",
    signature(object = "SimLogis"),
    function (object)
    {
		lab <- makeLabels(object)
		cat(paste("Random Logistic Distribution Object: ", lab, ".\n", sep=""))
    }
)
# Logistic Distribution
# Attributes: location = location parameter, scale = scale parameter (> 0)

setMethod("summaryShort",
    signature(object = "SimNbinom"),
    function (object)
    {
		lab <- makeLabels(object)
		cat(paste("Random Negative Binomial Distribution Object: ", lab, ".\n", sep=""))
    }
)
# Negative Binomial Distribution
# Attributes: size = Target for number of sucessful trials (> 0), prob = probability of each trials (0 < p < 1)

setMethod("summaryShort",
    signature(object = "SimPois"),
    function (object)
    {
		lab <- makeLabels(object)
		cat(paste("Random Poisson Distribution Object: ", lab, ".\n", sep=""))
    }
)
# Poisson Distribution
# Attributes: lambda = mean and variance (> 0)

setMethod("summaryShort",
    signature(object = "SimT"),
    function (object)
    {
		lab <- makeLabels(object)
		cat(paste("Random t Distribution Object: ", lab, ".\n", sep=""))
    }
)
# Student t Distribution
# Attributes: df = degree of freedom (> 0), ncp = non-centrality parameter

setMethod("summaryShort",
    signature(object = "SimWeibull"),
    function (object)
    {
		lab <- makeLabels(object)
		cat(paste("Random Weibull Distribution Object: ", lab, ".\n", sep=""))
    }
)
# Weibull Distribution
# Attributes: shape = shape parameter, scale = scale parameter (> 0)

################################################################################
