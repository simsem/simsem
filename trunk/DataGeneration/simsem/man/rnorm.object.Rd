\name{rnorm.object}
\alias{rnorm.object}
\title{
    Create random normal distribution object
}
\description{
    Create random normal distribution object. Random normal distribution object will save mean and standard deviation parameter. This will use in specifying parameters that distributed as normal distribution.
}
\usage{
rnorm.object(Mean, SD)
}
\arguments{
  \item{Mean}{
    Desired population mean
}
  \item{SD}{
    Desired population standard deviation
}
}
\value{
    \item{Rnorm} {Random Normal Distribution object that save the specified parameters}
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\examples{
    n02 <- rnorm.object(0, 0.2)
    run(n02)
}
