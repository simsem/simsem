\name{vectorizeObject}
\alias{vectorizeObject}
\alias{vectorizeObject-methods}
\alias{vectorizeObject,ANY,ANY-method}
\alias{vectorizeObject,vector,vector-method}
\alias{vectorizeObject,matrix,matrix-method}
\alias{vectorizeObject,VirtualRSet,SimLabels-method}
\alias{vectorizeObject,MatrixSet,SimGenLabels-method}
\title{
	Change an object to a vector with labels
}
\description{
	Change an object to a vector with labels
}
\usage{
vectorizeObject(object, labels, ...)
}
\arguments{
  \item{object}{
	The object to be vectorized
}
  \item{labels}{
	The labels of each element in the object
}
  \item{\dots}{
	The other additional arguments, such as whether an object is symmetric matrix
}
}
\section{Methods}{
	\describe{
	\item{signature(object="vector", labels="vector")}{
		This function will select elements in the object that the corresponding elements in labels are not \code{NA} and give the name for it. 
	}
	\item{signature(object="matrix", labels="matrix")}{
		This function will select elements in the object that the corresponding elements in labels are not \code{NA}, give the name for it, and then transform it to vector. The additional arguement is \code{symmetric}, which is \code{TRUE} if the matrix is symmetric matrix
	}
	\item{signature(object="VirtualRSet", labels="SimLabels")}{
		This function will vectorize every matrix or vector in the object and combine them together to a single vector. 
	}
	\item{signature(object="MatrixSet", labels="SimGenLabels")}{
		This function will vectorize every matrix or vector in the object and combine them together to a single vector. 
	}
}}
\value{
	A vector with labels
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
# No example
}
