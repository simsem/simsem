\name{simLnorm}
\alias{simLnorm}
\title{
    Create random log normal distribution object
}
\description{
    Create random log normal distribution object. Random log normal distribution object will save the mean and standard deviation (in log scale) parameters. 
}
\usage{
simLnorm(meanlog = 0, sdlog = 1)
}
\arguments{
  \item{meanlog}{
    The mean in log scale
}
  \item{sdlog}{
    The standard deviation in log scale
}
}
\value{
    \item{SimLnorm}{Random Log Normal Distribution object (\code{\linkS4class{SimLnorm}}) that save the specified parameters}
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
    lognorm <- simLnorm(0, exp(1))
    run(lognorm)
	summary(lognorm)
}