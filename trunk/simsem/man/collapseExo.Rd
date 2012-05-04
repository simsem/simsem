\name{collapseExo}
\alias{collapseExo}
\title{
	Collapse all exogenous variables and put all in endogenous side only.
}
\description{
	Collapse all exogenous variables and put all in endogenous side only.
}
\usage{
collapseExo(object, value = 0, label = FALSE)
}
\arguments{
  \item{object}{
	The target \code{\linkS4class{VirtualRSet}} that has exogenous side.
}
  \item{value}{
	The value to be filled in non-specified elements in combining matrices together
}
  \item{label}{
	Keep row and column names if \code{TRUE}.
}
}
\value{
	The combined \code{\linkS4class{VirtualRSet}} class
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
# No example
}
