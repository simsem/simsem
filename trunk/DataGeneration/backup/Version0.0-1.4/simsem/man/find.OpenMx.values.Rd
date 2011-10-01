\name{find.OpenMx.values}
\alias{find.OpenMx.values}
\title{
	Rearrange starting values such that it is appropriate for OpenMx matrix specification (Internal)
}
\description{
	Will combine both starting values and fixed value as the values command in OpenMx matrix
}
\usage{
find.OpenMx.values(Parameters, Starting.Values)
}
\arguments{
  \item{Parameters}{ Any objects that describe parameters.
}
  \item{Starting.Values}{ Any fixed values object that describe starting values
}
}
\value{ Return object of constants that describe both starting values and fixed values.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\note{
    (Internal Function) Working for (\code{vector}, \code{vector}), (\code{matrix}, \code{matrix}), or (\code{freeParamSet}, \code{reducedMatrixSet}).
}
\examples{
#parameter <- c(NA, NA, 0, 0)
#starting.values <- c(2, 5, 0, 0)
#find.OpenMx.Values(parameter, starting.values)
}
