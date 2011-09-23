\name{runif.object}
\alias{runif.object}
\title{
    Create random uniform distribution object
}
\description{
    Create random uniform distribution object. Random uniform distribution object will save mean and standard deviation parameter. This will use in specifying parameters that distributed as normal distribution.
}
\usage{
runif.object(Lower, Upper)
}
\arguments{
  \item{Lower}{ Lower bound of the distribution
}
  \item{Upper}{ Upper bound of the distribution
}
}
\value{
    \item{Runif}{Random Uniform Distribution object that save the specified parameters}
}
\author{
Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\examples{
u1 <- runif.object(-0.1, 0.1)
run(u1)
}