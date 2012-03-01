\name{simT}
\alias{simT}
\title{
    Create random t distribution object
}
\description{
    Create random t distribution object. Random t distribution object will save the degree of freedom and the non-centrality parameters. 
}
\usage{
simT(df, ncp = 0)
}
\arguments{
  \item{df}{
    The degree of freedom
}
  \item{ncp}{
    The non-centrality parameter
}
}
\value{
    \item{SimT}{Random t Distribution object (\code{\linkS4class{SimT}}) that save the specified parameters}
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
    nct82 <- simT(8, ncp=2)
    run(nct82)
	summary(nct82)
}