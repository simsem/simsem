\name{starting.values}
\alias{starting.values}
\title{
	Find starting values of free parameters (Internal)
}
\description{
	Find starting values of free parameters based on pre-specified starting values. If the pre-specified starting values are numbers, the function will use that values. If they are distribution object, this function will randomly draw from the distribution 10 times and take the average of those values.
}
\usage{
starting.values(object, trial, ...)
}
\arguments{
  \item{object}{
	A specified \code{simMatrix}, \code{simVector}, or \code{simMatrixSet} that wish to find starting values
}
  \item{trial}{
	Number of random drawn to find starting values of distribution objects
}
  \item{\dots}{
	Other arguments
}
}
\details{
	This function can be used for \code{simMatrix}, \code{simVector}, and \code{simMatrixSet}.
}
\value{
	\code{matrix}, \code{vector}, or \code{matrixSet} of starting values
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\note{
	Internal Function
}
\examples{
#u89 <- runif.object(0.8, 0.9)
#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#LX <- matrix.object(loading, "u89")

#latent.cor <- matrix(NA, 2, 2)
#diag(latent.cor) <- 1
#PH <- sym.matrix.object(latent.cor, 0.5)

#error.cor <- matrix(0, 6, 6)
#diag(error.cor) <- 1
#TD <- sym.matrix.object(error.cor)

#CFA.Model <- matrix.CFA.object(LX = LX, PH = PH, TD = TD)
#starting.values(LX, 10)
#result <- starting.values(CFA.Model, 10)
#summary(result)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
