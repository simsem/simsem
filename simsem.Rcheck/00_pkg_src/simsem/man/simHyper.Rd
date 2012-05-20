\name{simHyper}
\alias{simHyper}
\title{
    Create random hypergeometric distribution object
}
\description{
    Create random hypergeometric distribution object. Random hypergeometric distribution object will save the numbers of successes, failures, and drawns parameters. 
}
\usage{
simHyper(m, n, k)
}
\arguments{
  \item{m}{
    The number of successes
}
  \item{n}{
    The number of failures
}
  \item{k}{
    The number of drawns
}
}
\value{
    \item{SimHyper}{Random Hypergeometric Distribution object (\code{\linkS4class{SimHyper}}) that save the specified parameters}
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
    hyp <- simHyper(20, 5, 10)
    run(hyp)
	summary(hyp)
}