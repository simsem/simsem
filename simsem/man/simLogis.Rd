\name{simLogis}
\alias{simLogis}
\title{
    Create random logistic distribution object
}
\description{
    Create random logistic distribution object. Random logistic distribution object will save the location and scale parameters. 
}
\usage{
simLogis(location = 0, scale = 1)
}
\arguments{
  \item{location}{
    The location parameter
}
  \item{scale}{
    The scale parameter
}
}
\value{
    \item{SimLogis}{Random Logistic Distribution object (\code{\linkS4class{SimLogis}}) that save the specified parameters}
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
    logis <- simLogis(0, 1)
    run(logis)
	summary(logis)
}