\name{divideObject}
\alias{divideObject}
\alias{divideObject-methods}
\alias{divideObject,ANY-method}
\alias{divideObject,vector,numeric-method}
\alias{divideObject,matrix,numeric-method}
\alias{divideObject,MatrixSet,numeric-method}
\title{
	Make a division on each element of the object 
}
\description{
	Make a division on each element of the object 
}
\usage{
divideObject(object, constant, ...)
}
\arguments{
  \item{object}{
	A desired object to be divided
}
  \item{constant}{
	A divisor, which is a number
}
  \item{\dots}{
	Other arguments, such as whether the matrix is a correlation matrix
}
}
\section{Methods}{
	\describe{
	\item{signature(object="vector", constant="numeric")}{
		Divide each element in the \code{vector}. If the \code{vector} is \code{\linkS4class{NullVector}}, it will do nothing.
	}
	\item{signature(object="matrix", constant="numeric")}{
		Divide each element in the \code{matrix}. If the \code{matrix} is \code{\linkS4class{NullMatrix}}, it will do nothing. The additional argument is \code{correlation}. Use \code{correlation=TRUE} to show that the target object is a correlation matrix and the function will keep the diagonal elements as 1. 
	}
	\item{signature(object="MatrixSet", constant="numeric")}{
		Divide all matrices and vectors in the \code{\linkS4class{MatrixSet}}
	}
}}
\value{
	The divided object
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
# No example
}
