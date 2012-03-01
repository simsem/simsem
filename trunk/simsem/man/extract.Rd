\name{extract}
\alias{extract}
\alias{extract-methods}
\alias{extract,vector-method}
\alias{extract,matrix-method}
\alias{extract,VirtualRSet-method}
\title{
	Extract a part of an object
}
\description{
	Extract a part of an object, such as selecting only a subset of variables from a model specification
}
\usage{
extract(object, ...)
}
\arguments{
  \item{object}{
	The extracted object
}
  \item{\dots}{
	The specification of the extracted part
}
}
\section{Methods}{
	\describe{
	\item{\code{signature(object = "vector")}}{
		The additional argument is \code{pos}. This is the position of the extracted vector.
	}
	\item{\code{signature(object = "matrix")}}{
		The additional arguments are \code{row} and \code{col}. These are the positions of row and column. This method will prevent the matrix transforming to a vector if the number of rows or columnes is 1.
	}
}}
\value{
	The extracted object
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	This is the list of classes that can use the \code{extract} method.
	\itemize{
		\item \code{\linkS4class{SimMatrix}}
		\item \code{\linkS4class{SimVector}}
		\item \code{\linkS4class{SimSet}}
		\item \code{\linkS4class{SimDataDist}}
	}
}
\examples{
extract(1:10, c(4, 5))
extract(diag(3), 1, 2:3)
}
