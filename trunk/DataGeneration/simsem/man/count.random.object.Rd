\name{count.random.object}
\alias{count.random.object}
\title{
	Count how many free parameters is a specified object (Internal)
}
\description{
	Count how many free parameters is a specified object (Internal)
}
\usage{
count.random.object(object)
}
\arguments{
  \item{object}{
	A desired object to count the number of free parameters
}
}
\value{
	\code{value} as number of objects
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\note{
	Internal function
}
\examples{
	n02 <- rnorm.object(0, 0.2)
	AL <- vector.object(rep(NA, 4), "n02")
	count.random.object(AL)
	
	loading <- matrix(0, 9, 3)
	loading[1:3, 1] <- loading[4:6, 2] <- loading[7:9, 3] <- NA
	LX <- matrix.object(loading, "n02")
	count.random.object(LX)
}