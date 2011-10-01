\name{contain}
\alias{contain}
\title{
	Check whether an element is in a vector (Internal)
}
\description{
	Check whether an element is in a vector
}
\usage{
contain(element, Vector)
}
\arguments{
  \item{element}{
	Desired element that would like to be searched
}
  \item{Vector}{
	Desired object matched with the element
}
}
\value{
	TRUE if the element is in the vector
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\note{
	Internal Function
}
\examples{
	#contain(0, 1:3)
	#contain(1, 1:3)
}
