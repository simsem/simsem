\name{kurtosis}
\alias{kurtosis}
\alias{kurtosis-methods}
\alias{kurtosis,vector-method}
\title{
	Finding excessive kurtosis
}
\description{
	Finding excessive kurtosis (g2) of an object
}
\usage{
kurtosis(object, ...)
}
\arguments{
  \item{object}{
	An object used to find a excessive kurtosis, which can be a vector or a distribution object.
}
  \item{...}{
	Other arguments such as the option for reversing the distribution.
  }
}
\value{
	A value of an excessive kurtosis with a test statistic if the sample excessive kurtosis is computed.
}
\details{
	The excessive kurtosis computed is g2. See the Wolfram Mathworld for the excessive kurtosis detail.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
kurtosis(1:5)
}
