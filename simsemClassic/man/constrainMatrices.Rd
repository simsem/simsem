\name{constrainMatrices}
\alias{constrainMatrices}
\alias{constrainMatrices-methods}
\alias{constrainMatrices,ANY,ANY-method}
\alias{constrainMatrices,MatrixSet,SimEqualCon-method}
\title{
	Impose an equality constraint in an object
}
\description{
	Impose an equality constraint in an object
}
\usage{
constrainMatrices(object, SimEqualCon, ...)
}
\arguments{
  \item{object}{
	The desired object that would like to be constrained, such as the list of parameter matrices
}
  \item{SimEqualCon}{
	The equality constraint that saves the desired constraints.
}
  \item{\dots}{
	The additional arguments
}
}
\section{Methods}{
	\describe{
	\item{signature(object="MatrixSet", SimEqualCon="SimEqualCon")}{
		This function will impose constraint codes in the input object such that the number of the first element in the constraint will be copied to other elements in the constraint. 		
	}
}}
\value{
	The constrained object
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
# No example
}
