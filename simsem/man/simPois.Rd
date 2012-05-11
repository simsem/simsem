\name{simPois}
\alias{simPois}
\title{
    Create random Poisson distribution object
}
\description{
    Create random Poisson distribution object. Random Poisson distribution object will save the lambda parameters. 
}
\usage{
simPois(lambda)
}
\arguments{
  \item{lambda}{
    The lambda parameter (equal to the expected value of mean and variance)
}
}
\value{
    \item{SimPois}{Random Poisson Distribution object (\code{\linkS4class{SimPois}}) that save the specified parameters}
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
    pois5 <- simPois(5)
    run(pois5)
	summary(pois5)
}