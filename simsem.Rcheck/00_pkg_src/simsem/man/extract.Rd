\name{extract}
\alias{extract}
\alias{extract-methods}
\alias{extract,vector-method}
\alias{extract,matrix-method}
\alias{extract,VirtualRSet-method}
\alias{extract,data.frame-method}
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
	\item{\code{signature(object = "data.frame")}}{
		Extract elements from a data.frame. There are several additional arguments. First, if \code{yOnly} is \code{TRUE}, then the result will provide only Y side. Second, \code{y} is the index of indicators in Y side to be extracted. Third, \code{e} is the index of factors in Y side to be extracted. Fourth, \code{x} is the index of the indicators in X side to be extracted. Fifth, \code{k} is the index of the factors in X side to be extracted. Finally, \code{keepOriginalName} is to not reorder the extracted variables.
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
