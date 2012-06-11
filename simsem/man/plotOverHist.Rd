\name{plotOverHist}
\alias{plotOverHist}
\title{
Plot multiple overlapping histograms
}
\description{
Plot multiple overlapping histograms and find the cutoff values if not specified
}
\usage{
plotOverHist(altObject, nullObject, cutoff=NULL, usedFit=NULL, alpha=alpha)
}
\arguments{
  \item{altObject}{
	The result object (\code{data.frame}) saves the simulation result of fitting the hypothesized model when the hypothesized model is \code{FALSE}.
}
  \item{nullObject}{
	The result object (\code{data.frame}) saves the simulation result of fitting the hypothesized model when the hypothesized model is \code{TRUE}. 
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
