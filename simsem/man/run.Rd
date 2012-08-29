\name{run}
\alias{run}
\alias{run-methods}
\alias{run,ANY-method}
\title{
Run a particular object in \code{simsem} package.
}
\description{
Run a particular object such as running any distribution objects to create number.
}
\usage{
run(object, ...)
}
\arguments{
  \item{object}{
    \sQuote{simsem} object
}
  \item{\dots}{
    any additional arguments, listed below.
}
}
\value{
    \item{object}{depends on particular object}
}
\section{Methods}{
	\describe{
	\item{\code{signature(object = "SimNorm")}}{
		No additional arguments. The function will random draw a number from normal distribution object.
	}
	\item{\code{signature(object = "SimUnif")}}{
		No additional arguments. The function will random draw a number from uniform distribution object.
	}
}}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	This is the list of classes that can use \code{run} method.
	\enumerate{
		\item \code{\linkS4class{SimNorm}}
		\item \code{\linkS4class{SimUnif}}
	}
}
\examples{
n02 <- simNorm(0, 0.2)
run(n02)
}
\keyword{ run }