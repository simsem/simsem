\name{simChisq}
\alias{simChisq}
\title{
    Create random chi-squared distribution object
}
\description{
    Create random chi-squared distribution object. Random chi-squared distribution object will save the degree of freedom and the non-centrality parameters. 
}
\usage{
simChisq(df, ncp=0)
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
    \item{SimChisq}{Random Chi-squared Distribution object (\code{\linkS4class{SimChisq}}) that save the specified parameters}
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
    chi5 <- simChisq(5)
    run(chi5)
	summary(chi5)
}