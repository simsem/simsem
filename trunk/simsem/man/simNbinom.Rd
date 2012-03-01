\name{simNbinom}
\alias{simNbinom}
\title{
    Create random negative binomial distribution object
}
\description{
    Create random negative binomial distribution object. Random negative binomial distribution object will save the target number of successful trials and the probability of successes parameters. 
}
\usage{
simNbinom(size, prob)
}
\arguments{
  \item{size}{
    The target number of successful trials
}
  \item{prob}{
    The probability of successes
}
}
\value{
    \item{SimNbinom}{Random Negative Binomial Distribution object (\code{\linkS4class{SimNbinom}}) that save the specified parameters}
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
    nbinom <- simNbinom(5, 0.25)
    run(nbinom)
	summary(nbinom)
}