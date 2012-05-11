\name{combineObject}
\alias{combineObject}
\alias{combineObject-methods}
\alias{combineObject,ANY,ANY-method}
\alias{combineObject,SimMatrix,SimMatrix-method}
\alias{combineObject,SimVector,SimVector-method}
\alias{combineObject,vector,vector-method}
\alias{combineObject,matrix,matrix-method}
\alias{combineObject,MatrixSet,MatrixSet-method}
\alias{combineObject,SimParam,list-method}
\title{
	Combine by summing or binding two objects together.
}
\description{
	Combine by summing or binding two objects together.
}
\usage{
combineObject(object1, object2, ...)
}
\arguments{
  \item{object1}{
	The first object
}
  \item{object2}{
	The second object
}
  \item{\dots}{
	The additional arguments
}
}
\section{Methods}{
	\describe{
	\item{signature(object1="SimMatrix", object2="SimMatrix")}{
		This function will combine two \code{\linkS4class{SimMatrix}} together by,  1) If a parameter/starting values of an element is specified, the combined object will be free parameters with the starting value. If both objects have parameter/starting values, one of \code{object1} will be used. 2) If both are fixed, the fixed value of \code{object1} will be used. 		
	}
	\item{signature(object1="SimVector", object2="SimVector")}{
		This function will combine two \code{\linkS4class{SimVector}} together by, 1) If a parameter/starting values of an element is specified, the combined object will be free parameters with the starting value. If both objects have parameter/starting values, one of \code{object1} will be used. 2) If both are fixed, the fixed value of \code{object1} will be used. 		
	}
	\item{signature(object1="vector", object2="vector")}{
		This function is used to combine two vectors. If both are null vectors, it will return null vectors. If \code{object2} is null vector, it will return \code{object1}. If both objects are not null, it will return the sum of both vectors. 		
	}
	\item{signature(object1="matrix", object2="matrix")}{
		This function is used to combine two matrices. If both are null matrices, it will return null matrices. If \code{object2} is null matrix, it will return \code{object1}. If both objects are not null, it will return the sum of both objects. If both objects are correlation matrices, it will retain diagonal elements of 1. The additional argument is \code{correlation}, which, if \code{TRUE}, the diagonal elements are always fixed to 1.		
	}
	\item{signature(object1="MatrixSet", object2="MatrixSet")}{
		This function is used to combine two matrices. If both are null matrices, it will return null matrices. If object2 is null matrix, it will return \code{object1}. If both objects are not null, it will return the sum of both objects. If both objects are correlation matrices, it will retain diagonal elements of 1. 	
	}
	\item{signature(object1="SimParam", object2="list")}{
		The \code{object1} is \code{\linkS4class{SimParam}} of that saves all free parameters and values of fixed parameters. The \code{object2} is the lavaan estimates or standard error. This function will find any free parameters in the \code{object1} and search for appropriate number from \code{object2} and plug in the free parameters. This function will return \code{\linkS4class{SimRSet}} containing parameter estimates or standard errors.	
	}
}}
\value{
	The combined object
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
# No example
}
