\name{plotPowerFitDf}
\alias{plotPowerFitDf}
\title{
Plot sampling distributions of fit indices that visualize power of rejecting datasets underlying misspecified models
}
\description{
This function will plot sampling distributions of fit indices that visualize power in rejecting the misspecified models. This function is similar to the \code{\link{plotPowerFit}} function but the input distributions are \code{data.frame}.
}
\usage{
plotPowerFitDf(altObject, nullObject = NULL, cutoff = NULL, usedFit = NULL, alpha = 0.05, x = NULL, xval = NULL, useContour = TRUE, logistic = TRUE)
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
  \item{logistic}{
	If \code{logistic} is \code{TRUE} and the varying parameter exists (e.g., sample size or percent missing), the plot based on logistic regression predicting the significance by the varying parameters is preferred. If \code{FALSE}, the overlaying scatterplot with a line of cutoff is plotted.
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
