\name{simBeta}
\alias{simBeta}
\title{
    Create random beta distribution object
}
\description{
    Create random beta distribution object. Random beta distribution object will save shape parameters and non-centrality parameter. 
}
\usage{
simBeta(shape1, shape2, ncp=0)
}
\arguments{
  \item{shape1}{
    The first shape parameter (alpha)
}
  \item{shape2}{
    The second shape parameter (beta)
}
  \item{ncp}{
    Non-centrality parameter
}
}
\value{
    \item{SimBeta}{Random Beta Distribution object (\code{\linkS4class{SimBeta}}) that save the specified parameters}
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
    b11 <- simBeta(1, 1)
    run(b11)
}