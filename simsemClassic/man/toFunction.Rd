\name{toFunction}
\alias{toFunction}
\alias{toFunction-methods}
\alias{toFunction,ANY-method}
\title{
	Export the distribution object to a function command in text that can be evaluated directly.
}
\description{
	Export the distribution object to a function command in text that can be evaluated directly.
}
\usage{
toFunction(x)
}
\arguments{
  \item{x}{
	The distribution object used to be transformed
}
}
\value{
	The expression that is ready to be evaluated.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\seealso{
	\code{\linkS4class{VirtualDist}} for the distribution object that can be transformed to a function
}
\examples{
u2 <- simUnif(-0.2, 0.2)
toFunction(u2)
}
