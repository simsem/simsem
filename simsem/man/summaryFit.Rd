\name{summaryFit}
\alias{summaryFit}
\alias{summaryFit-methods}
\alias{summaryFit,ANY-method}
\alias{summaryFit,SimResult-method}
\alias{summaryFit,SimResultParam-method}
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
	\code{\linkS4class{SimResult}} or \code{linkS4class{SimResultParam}} to be summarized
}
  \item{\dots}{
    any additional arguments, such as for the function with result object, \code{digits} argument is available to adjust digits in results, \code{alpha} is available to select a specific alpha for fit index cutoffs.
}
}
\value{
	A data frame that provides fit statistics cutoffs and means
	
	When \code{linkS4class{SimResult}} has fixed simulation parameters the first colmns are fit index cutoffs for values of alpha and the last column is the mean fit across all replications. Rows are
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
	
	When \code{linkS4class{SimResultParam}} is specified in the \code{object}, the reported fit indices are
	\itemize{
		\item {f0} Discrepancy Function
		\item {RMSEA} Root Mean Square Error of Approximation
		\item {SRMR} Standardized Root Mean Residual
	}
	See details in \code{\link{popDiscrepancy}} and \code{\link{popMisfitMACS}}
}
\author{
    Alexander M. Schoemann (University of Kansas; \email{schoemann@ku.edu})
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}

\seealso{
	\code{\linkS4class{SimResult}} for the result object input
	\code{\linkS4class{SimResultParam}} for the parameter result object input
}
\examples{
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- bind(loading, 0.7)
RPH <- binds(diag(1))
RTD <- binds(diag(6))
CFA.Model <- model(LY = LX, RPS = RPH, RTE = RTD, modelType="CFA")

# We make the examples running only 5 replications to save time.
# In reality, more replications are needed.
Output <- sim(5, n=500, CFA.Model)
summaryFit(Output)
summaryFit(Output, detail=TRUE)
}
