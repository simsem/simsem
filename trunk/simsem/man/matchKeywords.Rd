\name{matchKeywords}
\alias{matchKeywords}
\title{
	Search for the keywords and check whether the specified text match one in the name vector
}
\description{
	Search for the keywords and check whether the specified text match one in the name vector
}
\usage{
matchKeywords(Names, keywords)
}
\arguments{
  \item{Names}{
	The name of the searching object
}
  \item{keywords}{
	The name of the keywords vector that would like to matched
}
}
\value{
	The position of keywords in the vector. Return \code{0} if the names does not match the specified vector.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\examples{
# This function is not a public function.

# matchKeywords("ly", c("LY", "LX"))
}
