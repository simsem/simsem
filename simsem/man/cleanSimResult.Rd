\name{cleanSimResult}
\alias{cleanSimResult}
\title{
	Extract only converged replications in the result object
}
\description{
	Extract only the replications that are converegent in a result object (\code{\linkS4class{SimResult}})
}
\usage{
cleanSimResult(object, converged=NULL)
}
\arguments{
  \item{object}{
	The target result object (\code{\linkS4class{SimResult}})
}
  \item{converged}{
	The replications to be extracted. If \code{NULL}, the converged slot in the result object will be used
}
}
\value{
	The cleaned result object
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
# No example
}
