\name{getCondQtile}
\alias{getCondQtile}
\title{
	Get a quantile of a variable given values of predictors
}
\description{
Find a quantile of a variable. If the predictors are specified, the result will provide the conditional quantile given specified value of predictors. The \code{quantreg} package is used to find conditional quantile.
}
\usage{
getCondQtile(y, x=NULL, xval=NULL, df = 0, qtile = 0.5)
}
\arguments{
  \item{y}{
	The variable that users wish to find a quantile from
}
  \item{x}{
	The predictors variables. If \code{NULL}, the unconditional quantile of the \code{y} is provided.
}
  \item{xval}{
	The vector of predictors' values that users wish to find the conditional quantile from. If \code{"all"} is specified, the function will provide the conditional quantile of every value in \code{x}.
}
  \item{df}{
	The degree of freedom used in spline method in predicting the fit indices by the predictors. If \code{df} is 0, the spline method will not be applied. 
}
  \item{qtile}{
	The quantile rank.
}
}
\value{
	A (conditional) quantile value
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\link{getCutoff}} for finding fit indices cutoffs using conditional quantiles
	}
}
\examples{
# No example
}
