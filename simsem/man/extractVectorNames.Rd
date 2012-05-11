\name{extractVectorNames}
\alias{extractVectorNames}
\title{
	Extract a vector of parameter names based on specified elements
}
\description{
	Extract a vector of parameter names based on specified elements
}
\usage{
extractVectorNames(columnName, keep=NULL)
}
\arguments{
  \item{columnName}{
	A name representing vector elements (e.g., TY2) that will be used to be extracted
}
  \item{keep}{
	The elements of the vector that we need to keep
}
}
\value{
	A list that contains: 1) \code{columnName}: Original element names and 2) \code{newName}: Reordered element names.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
# The function is not public

#vec <- c("TY1", "TY2", "TY3", "TY4", "TY5", "TY6", "TY7")
#extractVectorNames(vec, 5:6)
}
