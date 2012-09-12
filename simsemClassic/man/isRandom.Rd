\name{isRandom}
\alias{isRandom}
\alias{isRandom-methods}
\alias{isRandom,ANY-method}
\alias{isRandom,SimMatrix-method}
\alias{isRandom,SimVector-method}
\alias{isRandom,SimSet-method}
\title{
	Check whether the object contains any random parameters
}
\description{
	Check whether the object contains any random parameters
}
\usage{
isRandom(object)
}
\arguments{
  \item{object}{
	The object to be checked
}
}
\section{Methods}{
	\describe{
	\item{signature(object="SimMatrix")}{
		Check whether the \code{\linkS4class{SimMatrix}} contains any random parameters
	}
	\item{signature(object="SimVector")}{
		Check whether the \code{\linkS4class{SimVector}} contains any random parameters
	}
	\item{signature(object="SimSet")}{
		Check whether the \code{\linkS4class{SimSet}} contains any random parameters
	}
	\item{signature(object="SimMisspec")}{
		Check whether the \code{\linkS4class{SimMisspec}} contains any random parameters. If the optimization method is used in the misspecification object (\code{optMisfit="min"} or \code{optMisfit="max"}), the misspecification object is said to be fixed because the optimized set will be the same across any fixed parameters.
	}
	\item{signature(object="SimData")}{
		Check whether the \code{\linkS4class{SimData}} contains any random actual or misspecified parameters
	}
}}
\value{
	\code{TRUE} if the object contains any random parameters
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
# No example
}
