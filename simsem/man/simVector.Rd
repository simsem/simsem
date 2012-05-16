\name{simVector}
\alias{simVector}
\title{
	Create simVector that save free parameters and starting values, as well as fixed values
}
\description{
	Create \code{\linkS4class{SimVector}} object that save free parameters and starting values, as well as fixed values. This will be used for model specification later, such as for factor mean vector or measurement error variance vector.
}
\usage{
simVector(free = NULL, value = NULL)
}
\arguments{
  \item{free}{
	Vector of free parameters. Use \code{NA} to specify free parameters. Use number as fixed value (including zero). If this argument is not specified, the information from the \code{value} argument is used. The positions in the \code{value} argument that are 0 or "" are fixed parameters as 0. The other positions are free parameters.
}
  \item{value}{
	Starting values. Can be either one element or vector with the same length as free parameter vector. Each element can be numbers (in either \code{as.numeric} or \code{as.character} format) or the name of distribution object \code{\linkS4class{VirtualDist}}.
}
}
\value{
	\code{\linkS4class{SimVector}} object that will be used for model specification later.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item See \code{\linkS4class{SimVector}} for the resulting object. 
		\item See \code{\link{simMatrix}} for creating simMatrix. 
		\item See \code{\link{symMatrix}} for creating symmetric simMatrix.
	}
}
\examples{
factor.mean <- rep(NA, 4)
AL <- simVector(factor.mean, 0)

n02 <- simNorm(0, 0.2)
factor.start <- rep("n02", 4)
KA <- simVector(factor.mean, factor.start)

start <- c(2, 0, 0, 1)
VE <- simVector(value=start)
}
