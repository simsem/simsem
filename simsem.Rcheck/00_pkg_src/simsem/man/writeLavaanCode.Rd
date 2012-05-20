\name{writeLavaanCode}
\alias{writeLavaanCode}
\title{
	Write a lavaan code given the matrices of free parameter
}
\description{
	Write a lavaan code given the matrices of free parameter
}
\usage{
writeLavaanCode(object, constraint, aux = NULL)
}
\arguments{
  \item{object}{
	A parameter object, \code{\linkS4class{SimParam}}
}
  \item{constraint}{
	An equality constraint
}
  \item{aux}{
	The names of auxiliary variables
}
}
\value{
	A string of lavaan code
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\examples{
# No example
}
