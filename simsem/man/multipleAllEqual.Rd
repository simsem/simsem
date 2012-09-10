\name{multipleAllEqual}
\alias{multipleAllEqual}
\title{
	Test whether all objects are equal
}
\description{
	Test whether all objects are equal. The test is based on the \code{\link{all.equal}} function.
}
\usage{
multipleAllEqual(...)
}
\arguments{
  \item{\dots}{
	The target objects 
}
}
\value{
	\code{TRUE} if all objects are equal.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
multipleAllEqual(1:5, 1:5, seq(2, 10, 2)/2) # Should be TRUE
multipleAllEqual(1:5, 1:6, seq(2, 10, 2)/2) # Should be FALSE
}
