\name{run}
\alias{run}
\alias{run-methods}
\alias{run,ANY-method}
\alias{run,NullSimMatrix-method}
\alias{run,NullSymMatrix-method}
\alias{run,NullSimVector-method}
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
	\item{\code{signature(object = "SimData")}}{
		The function will random data from simData. Users may add \code{N} argument to change sample size.
	}
	\item{\code{signature(object = "SimMatrix")}}{
		No additional arguments. The function will random parameters from simMatrix.
	}
	\item{\code{signature(object = "SimSet")}}{
		No additional arguments. The function will random parameters from set of simMatrixs and simVectors.
	}
	\item{\code{signature(object = "SimMisspec")}}{
		No additional arguments. The function will random parameters from set of simMatrixs and simVectors in model misspecification.
	}
	\item{\code{signature(object = "SimModel")}}{
		The function will run an analysis specified in the \code{\linkS4class{SimModel}} object. One additional required argument is the data (put it as the second argument)
	}
	\item{\code{signature(object = "SimVector")}}{
		No additional arguments. The function will random parameters from simVector.
	}
	\item{\code{signature(object = "SymMatrix")}}{
		No additional arguments. The function will random parameters from symmetric simMatrix.
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
		\item \code{\linkS4class{SimMatrix}}
		\item \code{\linkS4class{SymMatrix}}
		\item \code{\linkS4class{SimVector}}
		\item \code{\linkS4class{SimSet}}
		\item \code{\linkS4class{SimData}}
		\item \code{\linkS4class{SimModel}}
		\item \code{\linkS4class{SimMisspec}}
	}
}
\examples{
n02 <- simNorm(0, 0.2)
run(n02)
}
\keyword{ run }