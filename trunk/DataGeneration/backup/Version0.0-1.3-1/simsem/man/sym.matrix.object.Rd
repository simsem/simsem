\name{sym.matrix.object}
\alias{sym.matrix.object}
\title{
	Create symmetric matrix object that save free parameters and starting values, as well as fixed values
}
\description{
	Create \code{symMatrix} object that save free parameters and starting values, as well as fixed values. This will be used for model specification later, such as for factor residual correlation matrix or measurement error correlation matrix.
}
\usage{
sym.matrix.object(Matrix, name.dist.object = NULL)
}
\arguments{
  \item{Matrix}{
	Symmetric matrix of free parameters. Use NA to specify free parameters. Use number as fixed value (including zero). The input matrix need to be symmetric matrix.
}
  \item{name.dist.object}{
	Starting values. Can be either one element or matrix with the same dimension as free parameter matrix. Each element can be numbers (in either \code{as.numeric} or \code{as.character} format) or the name of distribution object \code{\linkS4class{simDist}}.
}
}
\value{
	\code{symMatrix} object that will be used for model specification later.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\seealso{
	See \code{\linkS4class{simDist}} for the resulting object. See \code{\link{matrix.object}} for creating matrix object and \code{\link{vector.object}} for vector object.
}
\examples{
latent.cor <- matrix(NA, 3, 3)
diag(latent.cor) <- 1
PH <- sym.matrix.object(latent.cor, 0.5)

u46 <- runif.object(0.4, 0.6)
factor.cor <- matrix(NA, 4, 4)
diag(factor.cor) <- 1
factor.cor.start <- matrix("u46", 4, 4)
factor.cor.start[1, 2] <- factor.cor.start[2, 1] <- "0.5"
PS <- sym.matrix.object(factor.cor, factor.cor.start)
}
