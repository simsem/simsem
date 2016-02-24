\name{findRecursiveSet}
\alias{findRecursiveSet}
\title{
	Group variables regarding the position in mediation chain
}
\description{
In mediation analysis, variables affects other variables as a chain. This function will group variables regarding the chain of mediation analysis.
}
\usage{
findRecursiveSet(beta)
}
\arguments{
  \item{beta}{
	The regression coefficient in path analysis.
}
}
\value{
	The list of set of variables in the mediation chain. The variables in position 1 will be the independent variables. The variables in the last variables will be the end of the chain.
}
\author{
    Sunthud Pornprasertmanit (\email{psunthud@gmail.com})
}
\seealso{
	\itemize{
		\item \code{\link{findPossibleFactorCor}} to find the possible position for latent correlation given a regression coefficient matrix
	}
}
\examples{
path <- matrix(0, 9, 9)
path[4, 1] <- path[7, 4] <- NA
path[5, 2] <- path[8, 5] <- NA
path[6, 3] <- path[9, 6] <- NA
path[5, 1] <- path[8, 4] <- NA
path[6, 2] <- path[9, 5] <- NA
findRecursiveSet(path)
}
