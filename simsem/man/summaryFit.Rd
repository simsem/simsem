\name{summaryFit}
\alias{summaryFit}
\title{
	Provide summary of model fit across replications
}
\description{
	This function will provide fit index cutoffs for values of alpha, and mean fit index values across all replications.
}
\usage{
summaryFit(object, alpha = NULL)
}
\arguments{
  \item{object}{
	\code{\linkS4class{SimResult}} to be summarized
}
  \item{alpha}{
	The alpha level used to find the fit indices cutoff. If there is no varying condition, a vector of different alpha levels can be provided.
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
}
\author{
    Alexander M. Schoemann (University of Kansas; \email{schoemann@ku.edu})
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\code{\linkS4class{SimResult}} for the result object input
}
\examples{
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LY <- bind(loading, 0.7)
RPS <- binds(diag(1))
RTE <- binds(diag(6))
CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA")

# We make the examples running only 5 replications to save time.
# In reality, more replications are needed.
Output <- sim(5, n=500, CFA.Model)

# Summarize the sample fit indices
summaryFit(Output)
}
