\name{match.keyword}
\alias{match.keyword}
\title{
	Search for the keywords and check whether the specified text match one in the vector (Internal)
}
\description{
	Search for the keywords and check whether the specified text match one in the vector
}
\usage{
match.keyword(Names, keywords)
}
\arguments{
  \item{Names}{
	Name of the searching object
}
  \item{keywords}{
	Name of the vector that would like to matched
}
}
\value{
	The position of keywords in the vector. 0 if the names does not match the specified vector.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\note{
	Internal Function
}
\examples{
	#match.keyword("LY", c("LY", "Ly", "ly", "LX", "Lx", "lx"))
}