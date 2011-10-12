\name{vector.object}
\alias{vector.object}
\title{
	Create vector object that save free parameters and starting values, as well as fixed values
}
\description{
	Create \code{\linkS4class{simVector}} object that save free parameters and starting values, as well as fixed values. This will be used for model specification later, such as for factor mean vector or measurement error variance vector.
}
\usage{
vector.object(Vector, name.dist.object = NULL)
}
\arguments{
  \item{Vector}{
	Vector of free parameters. Use NA to specify free parameters. Use number as fixed value (including zero). 
}
  \item{name.dist.object}{
	Starting values. Can be either one element or vector with the same length as free parameter vector. Each element can be numbers (in either \code{as.numeric} or \code{as.character} format) or the name of distribution object \code{\linkS4class{simDist}}.
}
}
\value{
	\code{simVector} object that will be used for model specification later.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\enumerate{
		\item See \code{\linkS4class{simVector}} for the resulting object. 
		\item See \code{\link{matrix.object}} for creating matrix object. 
		\item See \code{\link{sym.matrix.object}} for creating symmetric matrix object.
	}
}
\examples{
factor.mean <- rep(NA, 4)
AL <- vector.object(factor.mean, 0)

n02 <- rnorm.object(0, 0.2)
factor.start <- rep("n02", 4)
KA <- vector.object(factor.mean, factor.start)
}
