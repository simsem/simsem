\name{run}
\alias{run}
\alias{run-methods}
\alias{run,ANY-method}
\alias{run,nullSimMatrix-method}
\alias{run,nullSymMatrix-method}
\alias{run,nullSimVector-method}
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
	\item{\code{signature(object = "Rnorm")}}{
		No additional arguments. The function will random draw a number from normal distribution object.
	}
	\item{\code{signature(object = "Runif")}}{
		No additional arguments. The function will random draw a number from uniform distribution object.
	}
	\item{\code{signature(object = "simData")}}{
		The function will random data from data object. Users may add \code{N} argument to change sample size.
	}
	\item{\code{signature(object = "simMatrix")}}{
		No additional arguments. The function will random parameters from matrix object.
	}
	\item{\code{signature(object = "simMatrixSet")}}{
		No additional arguments. The function will random parameters from set of matrix objects and vector objects.
	}
	\item{\code{signature(object = "simMisspecifiedSet")}}{
		No additional arguments. The function will random parameters from set of matrix objects and vector objects in model misspecification.
	}
	\item{\code{signature(object = "simModel")}}{
		The function will run an analysis specified in the \code{\linkS4class{simModel}} object. One additional required argument is the data (put it as the second argument)
	}
	\item{\code{signature(object = "simVector")}}{
		No additional arguments. The function will random parameters from vector object.
	}
	\item{\code{signature(object = "symMatrix")}}{
		No additional arguments. The function will random parameters from symmetric matrix object.
	}
}}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	This is the list of classes that can use \code{run} method.
	\enumerate{
		\item \code{\linkS4class{Rnorm}}
		\item \code{\linkS4class{Runif}}
		\item \code{\linkS4class{simMatrix}}
		\item \code{\linkS4class{symMatrix}}
		\item \code{\linkS4class{simVector}}
		\item \code{\linkS4class{simMatrixSet}}
		\item \code{\linkS4class{simData}}
		\item \code{\linkS4class{simModel}}
		\item \code{\linkS4class{simMisspecifiedSet}}
	}
}
\examples{
n02 <- rnorm.object(0, 0.2)
run(n02)
}
\keyword{ run }