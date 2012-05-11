\name{simBinom}
\alias{simBinom}
\title{
    Create random binomial distribution object
}
\description{
    Create random binomial distribution object. Random binomial distribution object will save the number of trials and the probability of successes parameters. 
}
\usage{
simBinom(size, prob)
}
\arguments{
  \item{size}{
    The number of trials
}
  \item{prob}{
    The probability of successes
}
}
\value{
    \item{SimBinom}{Random Binomial Distribution object (\code{\linkS4class{SimBinom}}) that save the specified parameters}
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
    b55 <- simBinom(5, 0.5)
    run(b55)
	summary(b55)
}