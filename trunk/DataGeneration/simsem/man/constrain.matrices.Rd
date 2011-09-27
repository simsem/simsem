\name{constrain.matrices}
\alias{constrain.matrices}
\title{
	Impose equality constraint in an object (Internal)
}
\description{
	Impose equality constraint in an object
}
\usage{
constrain.matrices(object, simConstraint, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	Desired object that would like to be constrained
}
  \item{simConstraint}{
	\code{simConstraint} object specifying equality constraints
}
  \item{\dots}{
	Other argumetns
}
}
\details{
	Candidate objects are class \code{blankReducedMatrixSet}. This class is used in \code{freeParamSet}, \code{labelsSet}, and \code{reducedMatrixSet}.
}
\value{
	The objects with equality constraints imposed
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\note{
	Internal function
}
