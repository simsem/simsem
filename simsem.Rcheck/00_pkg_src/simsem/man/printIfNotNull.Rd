\name{printIfNotNull}
\alias{printIfNotNull}
\title{
	Provide basic summary of each object if that object is not NULL. 
}
\description{
	Provide basic summary of each object if that object is not NULL. This function is mainly used in the \code{summary} function from the \code{linkS4class{SimSet}} object.
}
\usage{
printIfNotNull(object, name=NULL)
}
\arguments{
  \item{object}{
	The target object to be printed, which can be \code{linkS4class{SimMatrix}}, \code{linkS4class{SymMatrix}}, or \code{linkS4class{SimVector}}.
}
  \item{name}{
	The name of the target object
}
}
\value{
	None. This function will print only.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\examples{
# This function is not public

# AL <- simVector(rep(NA, 5), "0")
# printIfNotNull(AL, "Factor mean")
}
