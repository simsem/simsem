\name{plotOverHist}
\alias{plotOverHist}
\title{
Plot multiple overlapping histograms
}
\description{
Plot multiple overlapping histograms and find the cutoff values if not specified
}
\usage{
plotOverHist(altObject, nullObject, cutoff=NULL, usedFit=NULL, alpha=0.05, 
cutoff2=NULL, cutoff3=NULL, cutoff4=NULL)
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
  \item{cutoff2}{
	Another vector of priori cutoffs for fit indices.
}
  \item{cutoff3}{
	A vector of priori cutoffs for fit indices for the \code{altObject}.
}
  \item{cutoff4}{
	Another vector of priori cutoffs for fit indices for the \code{altObject}.
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
