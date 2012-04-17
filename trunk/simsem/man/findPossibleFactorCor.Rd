\name{findPossibleFactorCor}
\alias{findPossibleFactorCor}
\title{
	Find the appropriate position for freely estimated correlation (or covariance) given a regression coefficient matrix
}
\description{
Find the appropriate position for freely estimated correlation (or covariance) given a regression coefficient matrix. The appropriate position is the pair of variables that are not causally related.
}
\usage{
findPossibleFactorCor(beta)
}
\arguments{
  \item{beta}{
	The regression coefficient in path analysis.
}
}
\value{
	The symmetric matrix containing the appropriate position for freely estimated correlation.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\link{findRecursiveSet}} to group variables regarding the position in mediation chain.
	}
}
\examples{
path <- matrix(0, 9, 9)
path[4, 1] <- path[7, 4] <- NA
path[5, 2] <- path[8, 5] <- NA
path[6, 3] <- path[9, 6] <- NA
path[5, 1] <- path[8, 4] <- NA
path[6, 2] <- path[9, 5] <- NA
findPossibleFactorCor(path)
}
