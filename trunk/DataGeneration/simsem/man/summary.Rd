\name{summary}
\alias{summary}
\title{
Summary all simsem objects
}
\description{
The function will summarize all information in desired objects
}
\usage{
summary(object, ...)
}
\arguments{
  \item{object}{
    The name of desired object
}
  \item{\dots}{
    Any additional arguments based on characteristics of each type of objects
}
}
\details{
    The function summarize all information that used specified inside particular object.
}
\author{
        Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}

\examples{
    u1 <- runif.object(-0.1, 0.1)
    summary(u1)
    
    n02 <- rnorm.object(0, 0.2)
    summary(n02)
}
\keyword{ summary }
