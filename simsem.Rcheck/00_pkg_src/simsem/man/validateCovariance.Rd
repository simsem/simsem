\name{validateCovariance}
\alias{validateCovariance}
\title{
	Validate whether all elements provides a good covariance matrix
}
\description{
	Validate whether all elements provides a good covariance matrix
}
\usage{
validateCovariance(resVar, correlation, totalVar = NULL)
}
\arguments{
  \item{resVar}{
	A vector of residual variances
}
  \item{correlation}{
	A correlation matrix
}
  \item{totalVar}{
	A vector of total variances
}
}
\value{
	Return \code{TRUE} if the covariance matrix is good
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\examples{
# No example
}
