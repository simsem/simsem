\name{simF}
\alias{simF}
\title{
    Create random F distribution object
}
\description{
    Create random F distribution object. Random F distribution object will save the numerator and denominator degrees of freedom and the non-centrality parameters. 
}
\usage{
simF(df1, df2, ncp = 0)
}
\arguments{
  \item{df1}{
    The numerator degree of freedom
}
  \item{df2}{
    The denominator degree of freedom
}
  \item{ncp}{
    The non-centrality parameter
}
}
\value{
    \item{SimF}{Random F Distribution object (\code{\linkS4class{SimF}}) that save the specified parameters}
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
    f27 <- simF(2, 7)
    run(f27)
	summary(f27)
}