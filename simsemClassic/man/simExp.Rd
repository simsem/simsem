\name{simExp}
\alias{simExp}
\title{
    Create random exponential distribution object
}
\description{
    Create random exponential distribution object. Random exponential distribution object will save the rate parameters. 
}
\usage{
simExp(rate = 1)
}
\arguments{
  \item{rate}{
    The rate parameter
}
}
\value{
    \item{SimExp}{Random Exponential Distribution object (\code{\linkS4class{SimExp}}) that save the specified parameters}
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
    exp2 <- simExp(2)
    run(exp2)
	summary(exp2)
}