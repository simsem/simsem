\name{plotPowerSig}
\alias{plotPowerSig}
\title{
Plot multiple logistic curves given a significance result matrix
}
\description{
This function will plot the significance results given the value of predictors. 
}
\usage{
plotPowerSig(sig, x = NULL, xval=NULL, mainName = NULL, useContour = TRUE)
}
\arguments{
  \item{sig}{
	The \code{data.frame} of a significance result, which contains only \code{TRUE} for significance and \code{FALSE} for not significance.
}
\item{x}{
	The \code{data.frame} of the predictor values. The number of rows of the \code{x} argument should be equal to the number of rows in the \code{object}.
}
\item{xval}{
	The values of predictor that researchers would like to find the fit indices cutoffs from.
}
  \item{mainName}{
	A vector of the titles of the graphs
}
  \item{useContour}{
	If there are two of sample size, percent completely at random, and percent missing at random are varying, the \code{plotCutoff} function will provide 3D graph. Contour graph is a default. However, if this is specified as \code{FALSE}, perspective plot is used.
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
		\item \code{\link{plotPower}} 
	}
}
\examples{
# No example
}
