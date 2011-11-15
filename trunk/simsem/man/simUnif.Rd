\name{simUnif}
\alias{simUnif}
\title{
    Create random uniform distribution object
}
\description{
    Create random uniform distribution object. Random uniform distribution object will save mean and standard deviation parameter. This will use in specifying parameters that distributed as normal distribution.
}
\usage{
simUnif(min, max)
}
\arguments{
  \item{min}{ Lower bound of the distribution
}
  \item{max}{ Upper bound of the distribution
}
}
\value{
    \item{SimUnif}{Random Uniform Distribution object (\code{\linkS4class{SimUnif}}) that save the specified parameters}
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\enumerate{
		\item \code{\linkS4class{SimUnif}} for the simResult.
		\item \code{\linkS4class{VirtualDist}} for other distribution objects.
	}
}
\examples{
u1 <- simUnif(-0.1, 0.1)
run(u1)
}