\name{interpolate}
\alias{interpolate}
\title{
	Find the value of one vector relative to a value of another vector by interpolation
}
\description{
	Find the value of the resulting vector that have the position similar to the value of the baseline vector. If the starting value in the baseline vector is in between two elements, the resulting value will be predicted by linear interpolation.
}
\usage{
interpolate(baselineVec, val, resultVec=NULL)
}
\arguments{
  \item{baselineVec}{
	The target vector to be used as a baseline. The resulting vector can be attached as the element names of this vector.
}
  \item{val}{
	The value relative to the baseline vector to be used for projecting the resulting value
}
  \item{resultVec}{
	The vector that the resulting value will be used to base their result form
}
}
\value{
	The interpolated value from the resulting vector relative to the value in the baseline vector
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
# No Example
}
