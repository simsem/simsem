\name{pValueVariedCutoff}
\alias{pValueVariedCutoff}
\title{
Find a p value when the cutoff is specified as a vector given the values of predictors
}
\description{
Find a \emph{p} value when the cutoff is specified as a vector given the values of predictors. 
}
\usage{
pValueVariedCutoff(cutoff, obtainedValue, revDirec = FALSE, x = NULL, xval = NULL)
}
\arguments{
  \item{cutoff}{
	A vector of values used to find \code{p} values. Each value in the vector should be the target value conditional (applicable) to each value of the predictors (\code{x}) respectively.
}
\item{obtainedValue}{
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
