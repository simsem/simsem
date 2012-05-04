\name{centralMoment}
\alias{centralMoment}
\title{
	Calculate central moments of a variable
}
\description{
	Calculate central moments of a variable
}
\usage{
centralMoment(x, ord, weight = NULL)
}
\arguments{
  \item{x}{
	A vector of a variable
}
  \item{ord}{
	The order of the moment (the maximum is 4).
}
  \item{weight}{
	A vector of weights of each case
}
}
\value{
	The central moment value
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
centralMoment(1:5, 2)
}
