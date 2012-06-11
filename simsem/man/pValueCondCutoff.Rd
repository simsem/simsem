\name{pValueCondCutoff}
\alias{pValueCondCutoff}
\title{
Find a p value when the target is conditional (valid) on a specific value of a predictor
}
\description{
Find a \emph{p} value when the target is conditional (valid) on a specific value of a predictor. That is, the target value is applicable only a given value of a predictor.
}
\usage{
pValueCondCutoff(target, dist, revDirec = FALSE, x = NULL, xval = NULL, df = 0)
}
\arguments{
  \item{target}{
	A target value used to find \code{p} values. 
}
\item{dist}{
	The comparison distribution, which can be a vector of numbers, a data frame, or a result object.
}
\item{revDirec}{
	A logical argument whether to reverse the direction of comparison. If \code{TRUE}, the proportion of the \code{dist} that is lower than \code{target} value is reported. If \code{FALSE}, the proportion of the \code{dist} that is higher than the \code{target} value is reported.
}
  \item{x}{
	the \code{data.frame} of the predictor values. The number of rows of the \code{x} argument should be equal to the number of rows in the \code{dist}
}
  \item{xval}{
	the values of predictor that researchers would like to find the fit indices cutoffs from.
}
  \item{df}{
	the degree of freedom used in spline method in predicting the fit indices by the predictors. If \code{df} is 0, the spline method will not be applied. 
}
}
\value{
	A vector of \emph{p} values based on the comparison.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\link{pValue}} 
	}
}
\examples{
# No example
}
