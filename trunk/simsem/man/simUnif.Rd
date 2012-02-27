\name{simUnif}
\alias{simUnif}
\title{
    Create random uniform distribution object
}
\description{
    Create random uniform distribution object. Random uniform distribution object will save mean and standard deviation parameter. 
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
	\itemize{
		\item \code{\linkS4class{VirtualDist}} for all distribution objects.
	}
}
\examples{
u1 <- simUnif(-0.1, 0.1)
run(u1)
summary(u1)
}