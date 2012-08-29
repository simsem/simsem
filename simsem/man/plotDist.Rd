\name{plotDist}
\alias{plotDist}
\alias{plotDist-methods}
\title{
	Plot a distribution of a distribution object or data distribution object
}
\description{
	Plot a distribution of a distribution object or data distribution object
}
\usage{
plotDist(object, ...)
}
\arguments{
  \item{object}{
	The object to plot a distribution 
}
  \item{...}{
	Other arguments. Please see a class-specific method.
}
}
\value{
	No return value. This function will plot a graph only.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimDataDist}} for plotting a data distribution object
		\item \code{\linkS4class{VirtualDist}} for plotting a distribution object
	}
}
\examples{
gamma11 <- simGamma(1, 1)
plotDist(gamma11)

chi <- simChisq(5)
#dataDist <- simDataDist(chi, chi)
#plotDist(dataDist)
}
