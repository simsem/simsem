\name{countFreeParameters}
\alias{countFreeParameters}
\alias{countFreeParameters-methods}
\alias{countFreeParameters,ANY-method}
\alias{countFreeParameters,SimMatrix-method}
\alias{countFreeParameters,SymMatrix-method}
\alias{countFreeParameters,SimVector-method}
\alias{countFreeParameters,SimSet-method}
\alias{countFreeParameters,matrix-method}
\alias{countFreeParameters,vector-method}
\alias{countFreeParameters,VirtualRSet-method}
\alias{countFreeParameters,SimEqualCon-method}
\alias{countFreeParameters,SimREqualCon-method}
\title{
	Count how many free parameters in the target object
}
\description{
	Count how many free parameters in the target object
}
\usage{
countFreeParameters(object, ...)
}
\arguments{
  \item{object}{
	A desired object that users wish to count the number of free parameters
}
  \item{\dots}{
	Additional arguments specific to each class
}
}
\section{Methods}{
	\describe{
	\item{signature(object="SimMatrix")}{
		Count the number of \code{NA} in free parameter matrix	
	}
	\item{signature(object="SymMatrix")}{
		Count the number of \code{NA} in upper diagonal elements in free parameter matrix
	}
	\item{signature(object="SimVector")}{
		Count the number of \code{NA} in the elements of a free parameter vector
	}
	\item{signature(object="SimSet")}{
		Count the number of \code{NA} in the all vector or matrix objects inside the class
	}
	\item{signature(object="matrix")}{
		Count the number of \code{NA} in the elements in a matrix (in symmetric matrix, the lower tri part is ignored)
	}
	\item{signature(object="vector")}{
		Count the number of \code{NA} in the elements in a vector
	}
	\item{signature(object="VirtualRSet")}{
		Count the number of \code{NA} in the elements in all vectors and matrices inside the class
	}
	\item{signature(object="SimEqualCon")}{
		Count the number of parameters reduced by this equality constraint
	}
	\item{signature(object="SimREqualCon")}{
		Count the number of parameters reduced by this equality constraint
	}
}}
\value{
	The number of free parameters
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
# No example
}
