\name{summary.short}
\alias{summary.short}
\alias{summary.short-methods}
\alias{summary.short,ANY-method}
\alias{summary.short,vector-method}
\alias{summary.short,matrix-method}
\alias{summary,SimFreeParam-method}
\alias{summary,SimLabels-method}
\alias{summary,MatrixSet-method}
\alias{summary,SimRSet-method}
\title{
	Provide short summary of an object.
}
\description{
	Provide short summary if it is available. Otherwise, it is an alias for \code{summary}.
}
\usage{
summary.short(object, ...)
}
\arguments{
  \item{object}{
	Desired object being described
}
  \item{\dots}{
    any additional arguments
}
}
\value{
	NONE. This function will print on screen only.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	This is the list of classes that can use \code{run} method.
	\enumerate{
		\item \code{\linkS4class{SimMatrix}}
		\item \code{\linkS4class{SimVector}}
	}
}
\examples{
u89 <- simUnif(0.8, 0.9)
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
LX <- simMatrix(loading, "u89")
summary.short(LX)
}
