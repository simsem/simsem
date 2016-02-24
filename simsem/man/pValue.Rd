\name{pValue}
\alias{pValue}
\title{
	Find p-values (1 - percentile) by comparing a single analysis output from the result object
}
\description{
	This function will provide \emph{p} value from comparing a \code{\linkS4class{lavaan}}) or a OpenMx result from the simulation result (in \code{\linkS4class{SimResult}}).
}
\usage{
pValue(target, dist, usedFit = NULL, nVal = NULL, pmMCARval = NULL, 
	pmMARval = NULL, df = 0)
}
\arguments{
  \item{target}{
	A value, multiple values, a lavaan object, or an OpenMx object used to find p values. This argument could be a cutoff of a fit index.
}
  \item{dist}{
	The comparison distribution, which can be a vector of numbers, a data frame, or a result object.
}
  \item{usedFit}{
	The vector of names of fit indices that researchers wish to find the \emph{p} value from.
}
  \item{nVal}{
	The sample size value that researchers wish to find the fit indices cutoffs from
}
  \item{pmMCARval}{
	The percent missing completely at random value that researchers wish to find the fit indices cutoffs from.
}
  \item{pmMARval}{
	The percent missing at random value that researchers wish to find the fit indices cutoffs from.
}
  \item{df}{
	The degree of freedom used in spline method in predicting the fit indices by the predictors. If \code{df} is 0, the spline method will not be applied.
}
}
\value{
	The \emph{p} values of fit indices are provided, as well as two additional values: \code{andRule} and \code{orRule}. The \code{andRule} is based on the principle that the model is retained only when all fit indices provide good fit. The proportion is calculated from the number of replications that have all fit indices indicating a better model than the observed data. The proportion from the \code{andRule} is the most stringent rule in retaining a hypothesized model. The \code{orRule} is based on the principle that the model is retained only when at least one fit index provides good fit. The proportion is calculated from the number of replications that have at least one fit index indicating a better model than the observed data. The proportion from the \code{orRule} is the most lenient rule in retaining a hypothesized model.
}
\details{
	In comparing fit indices, the \emph{p} value is the proportion of the number of replications that provide poorer fit (e.g., less CFI value or greater RMSEA value) than the analysis result from the observed data. 
}
\author{
    Sunthud Pornprasertmanit (\email{psunthud@gmail.com})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimResult}} to run a simulation study
	}
}
\examples{
\dontrun{
# Compare an analysis result with a result of simulation study
library(lavaan)
loading <- matrix(0, 9, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
targetmodel <- estmodel(LY=loading, modelType="CFA", indLab=paste("x", 1:9, sep=""))
out <- analyze(targetmodel, HolzingerSwineford1939)

loading.trivial <- matrix("runif(1, -0.2, 0.2)", 9, 3)
loading.trivial[is.na(loading)] <- 0
mismodel <- model.lavaan(out, std=TRUE, LY=loading.trivial)

# The actual number of replications should be much greater than 20.
simout <- sim(20, n=nrow(HolzingerSwineford1939), mismodel)

# Find the p-value comparing the observed fit indices against the simulated 
# sampling distribution of fit indices
pValue(out, simout)
}
}
