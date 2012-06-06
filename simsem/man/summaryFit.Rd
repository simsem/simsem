\name{summaryParam}
\alias{summaryParam}
\alias{summaryParam-methods}
\alias{summaryParam,ANY-method}
\alias{summaryParam,SimResult-method}

\title{
	Provide summary of model fit across replications
}
\description{
	This function will provide fit index cutoffs for values of alpha, and mean fit index values across all replications.
}
\usage{
summaryFit(object,...)
}
\arguments{
  \item{object}{
	\code{\linkS4class{SimResult}} object being described
}
  \item{\dots}{
    any additional arguments, such as for the function with result object, \code{digits} argument is available to adjust digits in results, \code{alpha} is available to select a specific alpha for fit index cutoffs.
}
}
\value{
	A data frame that provides fit statistics cutoffs and means
	
	When \code{linkS4class{SimResult}} has fixed simulation parameters the first colmns are fit index cutoffs for values of alpha and the last column is the mean fit across all replications. Rows are
	Chi     13.046   15.367   18.476   22.591    8.148
AIC   3200.737 3218.214 3255.511 3265.471 3129.529
BIC   3263.405 3280.882 3318.179 3328.139 3192.197
RMSEA    0.056    0.068    0.081    0.095    0.019
CFI      0.983    0.974    0.965    0.955    0.995
TLI      0.967    0.952    0.934    0.915    1.000
SRMR    
	\itemize{
		\item {Chi} Chi-square fit statistic
		\item {AIC} Akaike Information Criterion
		\item {BIC} Baysian Information Criterion
		\item {RMSEA} Root Mean Square Error of Approximation
		\item {CFI} Comparative Fit Index
		\item {TLI} Tucker-Lewis Index
		\item {SRMR} Standardized Root Mean Residual
	}
	When \code{linkS4class{SimResult}} has random simulation parameters (sample size or percent missing), columns are the fit indices listed above and rows are values of the random parameter.
}
\author{
    Alexander M. Schoemann (University of Kansas; \email{schoemann@ku.edu})
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}

\seealso{
	\code{\linkS4class{SimResult}} for the object input
}
\examples{
showClass("SimResult")
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- simMatrix(loading, 0.7)
RPH <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model <- simSetCFA(LY = LX, RPS = RPH, RTE = RTD)
SimData <- simData(CFA.Model, 500)
SimModel <- simModel(CFA.Model)
# We make the examples running only 5 replications to save time.
# In reality, more replications are needed.
Output <- simResult(5, SimData, SimModel)
summaryParam(Output)
summaryParam(Output, detail=TRUE)
}
