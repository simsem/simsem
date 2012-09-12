\name{plotScatter}
\alias{plotScatter}
\title{
Plot overlaying scatter plots visualizing the power of rejecting misspecified models
}
\description{
This function will find the fit indices cutoff values if not specified and then plot the fit indices value against the value of predictors. The plot will include the fit indices value of the alternative models, the fit indices value of the null model (if specified), and the fit indices cutoffs.  
}
\usage{
plotScatter(altObject, nullObject=NULL, cutoff=NULL, usedFit = NULL, x, alpha=0.05, df=5)
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
\item{x}{
	The \code{data.frame} of the predictor values. The number of rows of the \code{x} argument should be equal to the number of rows in the \code{object}.
}
  \item{alpha}{
	A priori alpha level
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
