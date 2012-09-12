\name{simCauchy}
\alias{simCauchy}
\title{
    Create random Cauchy distribution object
}
\description{
    Create random Cauchy distribution object. Random Cauchy distribution object will save the location and the scale parameters. 
}
\usage{
simCauchy(location = 0, scale = 1)
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
    \item{SimCauchy}{Random Cauchy Distribution object (\code{\linkS4class{SimCauchy}}) that save the specified parameters}
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
    c02 <- simCauchy(0, 2)
    run(c02)
	summary(c02)
}