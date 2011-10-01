\name{matrix.object}
\alias{matrix.object}
\title{
	Create matrix object that save free parameters and starting values, as well as fixed values
}
\description{
	Create \code{simMatrix} object that save free parameters and starting values, as well as fixed values. This will be used for model specification later, such as for factor loading matrix or regression coefficient matrix.
}
\usage{
matrix.object(Matrix, name.dist.object = NULL)
}
\arguments{
  \item{Matrix}{
	Matrix of free parameters. Use NA to specify free parameters. Use number as fixed value (including zero)
}
  \item{name.dist.object}{
	Starting values. Can be either one element or matrix with the same dimension as free parameter matrix. Each element can be numbers (in either \code{as.numeric} or \code{as.character} format) or the name of distribution object \code{\linkS4class{simDist}}.
}
}
\value{
	\code{simMatrix} object that will be used for model specification later.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\seealso{
	See \code{\linkS4class{simDist}} for the resulting object. See \code{\link{sym.matrix.object}} for creating symmetric matrix object and \code{\link{vector.object}} for vector object.
}
\examples{
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- matrix.object(loading, loadingValues)
summary(LX)
run(LX)

n65 <- rnorm.object(0.6, 0.05)
LY <- matrix.object(loading, "n65")
summary(LY)
run(LY)
}
