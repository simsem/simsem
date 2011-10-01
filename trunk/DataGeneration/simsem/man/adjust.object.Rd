\name{adjust.object}
\alias{adjust.object}
\title{
	Change an element in \code{simMatrix}, \code{symMatrix}, or \code{simVector}.
}
\description{
	This function will adjust an element in \code{simMatrix}, \code{symMatrix}, or \code{simVector}. The specified element may be set to be free parameter with number or distribution object as starting values. The element can be fixed to be a value (such as 0).
}
\usage{
adjust.object(target, simDist, position, constant.fixed)
}
\arguments{
  \item{target}{
	Target \code{simMatrix}, \code{symMatrix}, or \code{simVector} that you would like to adjusted.
}
  \item{simDist}{
	The name of distribution object that you would like to specify (put as character with single or double quotation) or number that represents fixed values or starting values.
}
  \item{position}{
	The position of element that you would like to adjust, such as \code{"c(1,2}"} is the row 1 and column 2 element of the specified matrix.
}
  \item{constant.fixed}{ 
	This argument is used when the \code{simDist} item was specified as number. If true (as default), the number is treated as fixed parameters. If false, the number is treated as a starting value and free the parameter.
}
}
\value{
	Return the input \code{simMatrix}, \code{symMatrix}, or \code{simVector} with adjusted element.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\seealso{
\code{\linkS4class{simMatrix}} for random parameter matrix, \code{\linkS4class{symMatrix}} for symmetric random parameter matrix, and \code{\linkS4class{simVector}} for random parameter vector.
}
\examples{
#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#LX <- matrix.object(loading, 0.7)
#summary(LX)
#run(LX)

#u34 <- runif.object(0.3, 0.4)
#LX <- adjust.object(LX, "u34", c(2, 1))
#summary(LX)
#run(LX)

#LX <- adjust.object(LX, 0, c(2,1))
#LX <- adjust.object(LX, 0.5, c(2,2), FALSE)
#summary(LX)
#run(LX)

#factor.mean <- rep(NA, 2)
#factor.mean.starting <- c(5, 2)
#AL <- vector.object(factor.mean, factor.mean.starting)
#run(AL)
#summary(AL)

#n01 <- rnorm.object(0, 1)
#AL <- adjust.object(AL, "n01", 2)
#run(AL)
#summary(AL)
}

