\name{tag.headers}
\alias{tag.headers}
\title{
	Name each element of specified matrices or vectors (Internal)
}
\description{
	This element will add names in each element of a vector or will add row and columns names of a matrix with variable or factor names
}
\usage{
tag.headers(object, ...)
}
\arguments{
  \item{object}{
	\code{blankReducedMatrixSet} which will be inherited by \code{freeParamSet}, \code{labelsSet}, and \code{reducedMatrixSet}
}
  \item{\dots}{
	Other arguments
}
}
\details{
	Y means indicators on Y-side. X means indicators on X-side. E means endogenous factors. K means exogenous factors.
}
\value{
	Object with tags on it.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\note{
	Internal Function
}
