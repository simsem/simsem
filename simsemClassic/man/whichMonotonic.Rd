\name{whichMonotonic}
\alias{whichMonotonic}
\title{
	Extract a part of a vector that is monotonically increasing or decreasing
}
\description{
	Extract a part of a vector that is monotonically increasing or decreasing. This function will go to the anchor value and extract the neighbor values that are monotonically increasing or decreasing.
}
\usage{
whichMonotonic(vec, ord=NULL, anchor=NULL)
}
\arguments{
  \item{vec}{
	The target vector to be extracted
}
  \item{ord}{
	The names of each element of the vector to be attached
}
  \item{anchor}{
	The position of the element to be anchored. The default value is the middle position.
}
}
\value{
	The monotonic part of a vector
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
# This is a private function.

# whichMonotonic(c(3, 4, 1, 2, 3, 5, 2, 1))
}
