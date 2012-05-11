\name{skew}
\alias{skew}
\alias{skew-methods}
\alias{skew,vector-method}
\title{
	Finding skewness
}
\description{
	Finding skewness (g1) of an object
}
\usage{
skew(object, ...)
}
\arguments{
  \item{object}{
	An object used to find a skewness, which can be a vector or a distribution object.
}
  \item{...}{
	Other arguments such as the option for reversing the distribution.
  }
}
\value{
	A value of a skewness with a test statistic if the sample skewness is computed.
}
\details{
	The skewness computed is g1. See the Wolfram Mathworld for the skewness detail.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
skew(1:5)
}
