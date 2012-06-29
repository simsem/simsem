\name{plotLogisticFit}
\alias{plotLogisticFit}
\title{
Plot multiple logistic curves for predicting whether rejecting a misspecified model
}
\description{
This function will find the fit indices cutoff values if not specified, then check whether the hypothesized model is rejected in each dataset, and plot the logistic curve given the value of predictors. 
}
\usage{
plotLogisticFit(altObject, nullObject=NULL, cutoff=NULL, 
usedFit=NULL, x, xval, alpha=0.05, useContour=TRUE, df=0)
}
\arguments{
  \item{altObject}{
	The result object (\code{data.frame}) saves the simulation result of fitting the hypothesized model when the hypothesized model is \code{FALSE}.
}
  \item{nullObject}{
	The result object (\code{data.frame}) saves the simulation result of fitting the hypothesized model when the hypothesized model is \code{TRUE}. This argument may be not specified if the \code{cutoff} is specified.
}
  \item{cutoff}{
	A vector of priori cutoffs for fit indices.
}
  \item{usedFit}{
	Vector of names of fit indices that researchers wish to plot.
}
  \item{alpha}{
	A priori alpha level
}
\item{x}{
	The \code{data.frame} of the predictor values. The number of rows of the \code{x} argument should be equal to the number of rows in the \code{object}.
}
\item{xval}{
	The values of predictor that researchers would like to find the fit indices cutoffs from.
}
  \item{useContour}{
	If there are two of sample size, percent completely at random, and percent missing at random are varying, the \code{plotCutoff} function will provide 3D graph. Contour graph is a default. However, if this is specified as \code{FALSE}, perspective plot is used.
}
  \item{df}{
	The degree of freedom used in spline method in predicting the fit indices by the predictors. If \code{df} is 0, the spline method will not be applied.
}
}
\value{
	NONE. Only plot the fit indices distributions.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\link{plotPowerFit}} 
	}
}
\examples{
# No example
}
