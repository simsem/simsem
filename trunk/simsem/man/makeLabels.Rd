\name{makeLabels}
\alias{makeLabels}
\alias{makeLabels-methods}
\alias{makeLabels,ANY-method}
\alias{makeLabels,vector-method}
\alias{makeLabels,matrix-method}
\alias{makeLabels,SimParam-method}
\alias{makeLabels,VirtualDist-method}
\title{
	Make parameter names for each element in matrices or vectors or the name for the whole object
}
\description{
	Make parameter names for each element in matrices or vectors or the name for the whole object
}
\usage{
makeLabels(object, ...)
}
\arguments{
  \item{target}{
	The target object to find labels
}
  \item{\dots}{
	Additional arguments
}
}
\section{Methods}{
	\describe{
	\item{signature(target="vector")}{
		There are two additional arguments in this function: 1) \code{name} and \code{package}. The \code{name} is the name of this object. The \code{package} is the style to build the labels. This function will create labels of each element by the object name followed by the number of elements in a vector.
	}
	\item{signature(target="matrix")}{
		There are two additional arguments in this function: 1) \code{name} and \code{package}. The \code{name} is the name of this object. The \code{package} is the style to build the labels. This function will create labels of each element by the object name followed by the numbers of rows and columns in a matrix.
	}
	\item{signature(target="SimParam")}{
		The only addition argument is \code{package}, which is the style to build the labels. This function will create labels of each element by the object name followed by the numbers of rows and columns (or the number of elements) in a matrix in every matrix (or a vector) in the free parameter object.
	}
	\item{signature(target="VirtualDist")}{
		This function will create a description of attributes. The additional argument is \code{digit}, which is the number of decimals.
	}
}}
\value{
	The labeled object.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
# No example
}
