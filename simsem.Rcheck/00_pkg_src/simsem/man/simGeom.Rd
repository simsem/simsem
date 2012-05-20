\name{simGeom}
\alias{simGeom}
\title{
    Create random geometric distribution object
}
\description{
    Create random geometric distribution object. Random geometric distribution object will save the probability of successes parameters. 
}
\usage{
simGeom(prob)
}
\arguments{
  \item{prob}{
    The probability of successes
}
}
\value{
    \item{SimGeom}{Random Geometric Distribution object (\code{\linkS4class{SimGeom}}) that save the specified parameters}
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{VirtualDist}} for all distribution objects.
	}
}
\examples{
    geom5 <- simGeom(0.05)
    run(geom5)
	summary(geom5)
}