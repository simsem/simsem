\name{simNorm}
\alias{simNorm}
\title{
    Create random normal distribution object
}
\description{
    Create random normal distribution object. Random normal distribution object will save mean and standard deviation parameter. This will use in specifying parameters that distributed as normal distribution.
}
\usage{
simNorm(mean, sd)
}
\arguments{
  \item{mean}{
    Desired population mean
}
  \item{sd}{
    Desired population standard deviation
}
}
\value{
    \item{SimNorm}{Random Normal Distribution object (\code{\linkS4class{SimNorm}}) that save the specified parameters}
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimNorm}} for the simResult.
		\item \code{\linkS4class{VirtualDist}} for other distribution objects.
	}
}
\examples{
    n02 <- simNorm(0, 0.2)
    run(n02)
}