\name{tagHeaders}
\alias{tagHeaders}
\alias{tagHeaders-methods}
\alias{tagHeaders,ANY-method}
\alias{tagHeaders,VirtualRSet-method}
\title{
	Tag names to each element
}
\description{
	This element of a vector will be tagged by the names of the vector with the position of the element. This element of a matrix will be tagged by the names of the matrix with the row and column positions of the matrix. 
}
\usage{
tagHeaders(object, ...)
}
\arguments{
  \item{object}{
	The object to be tagged
}
  \item{\dots}{
	The additional arguments
}
}
\section{Methods}{
	\describe{
	\item{signature(object="VirtualRSet")}{
		This element of a vector will be tagged by the names of the vector with the position of the element. This element of a matrix will be tagged by the names of the matrix with the row and column positions of the matrix. \emph{Y} means indicators on \emph{Y}-side. \emph{X} means indicators on \emph{X}-side. \emph{E} means endogenous factors. \emph{K} means exogenous factors.
	}
}}
\value{
	The object with the row, column, or element names.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
# No example
}
