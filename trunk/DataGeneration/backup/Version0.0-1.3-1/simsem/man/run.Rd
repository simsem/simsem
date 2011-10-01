\name{run}
\alias{run}
\title{
Run a particular object in simsem package.
}
\description{
Run a particular object such as running any distribution objects to create number.
}
\usage{
run(object, ...)
}
\arguments{
  \item{object}{
    \sQuote{simsem} object
%%     ~~Describe \code{object} here~~
}
  \item{\dots}{
    any additional arguments
%%     ~~Describe \code{\dots} here~~
}
}
\value{
    \item{object}{depends on particular object}
}
\author{
Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\examples{
n02 <- rnorm.object(0, 0.2)
run(n02)
}
\keyword{ run }