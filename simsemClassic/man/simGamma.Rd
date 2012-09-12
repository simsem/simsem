\name{simGamma}
\alias{simGamma}
\title{
    Create random gamma distribution object
}
\description{
    Create random gamma distribution object. Random gamma distribution object will save the shape and rate parameters. 
}
\usage{
simGamma(shape, rate = 1)
}
\arguments{
  \item{shape}{
    The shape parameter (alpha)
}
  \item{rate}{
    The rate parameter (beta)
}
}
\value{
    \item{SimGamma}{Random Gamma Distribution object (\code{\linkS4class{SimGamma}}) that save the specified parameters}
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
    g11 <- simGamma(1, 1)
    run(g11)
	summary(g11)
}