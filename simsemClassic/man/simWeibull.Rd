\name{simWeibull}
\alias{simWeibull}
\title{
    Create random Weibull distribution object
}
\description{
    Create random Weibull distribution object. Random Weibull distribution object will save the shape and scale parameters. 
}
\usage{
simWeibull(shape, scale = 1)
}
\arguments{
  \item{shape}{
    The shape parameter
}
  \item{scale}{
    The scale parameter
}
}
\value{
    \item{SimWeibull}{Random Weibull Distribution object (\code{\linkS4class{SimWeibull}}) that save the specified parameters}
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
    exWeibull <- simWeibull(2, 100)
    run(exWeibull)
	summary(exWeibull)
}