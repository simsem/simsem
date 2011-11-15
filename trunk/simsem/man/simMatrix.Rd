\name{simMatrix}
\alias{simMatrix}
\title{
	Create simMatrix that save free parameters and starting values, as well as fixed values
}
\description{
	Create \code{\linkS4class{SimMatrix}} object that save free parameters and starting values, as well as fixed values. 
	This will be used for model specification later, such as for factor loading matrix or regression coefficient matrix.
}
\usage{
simMatrix(free, param = NULL)
}
\arguments{
  \item{free}{
	Matrix of free parameters. Use NA to specify free parameters. Use number as fixed value (including zero)
}
  \item{param}{
	Starting values. Can be either one element or matrix with the same dimension as free parameter matrix. 
	Each element can be numbers (in either \code{as.numeric} or \code{as.character} format) or the name of distribution object \code{\linkS4class{VirtualDist}}.
}
}
\value{
	\code{\linkS4class{SimMatrix}} object that will be used for model specification later.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item See \code{\linkS4class{VirtualDist}} for the resulting object. 
		\item See \code{\link{symMatrix}} for creating symmetric simMatrix.
		\item See \code{\link{simVector}} for simVector.
	}
}
\examples{
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- simMatrix(loading, loadingValues)
summary(LX)
run(LX)

n65 <- simNorm(0.6, 0.05)
LY <- simMatrix(loading, "n65")
summary(LY)
run(LY)
}
