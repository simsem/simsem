\name{findFactorIntercept}
\alias{findFactorIntercept}
\title{
	Find factor intercept from regression coefficient matrix and factor total means
}
\description{
Find factor intercept from regression coefficient matrix and factor total means for latent variable models. In the path analysis model, this function will find indicator intercept from regression coefficient and indicator total means.
}
\usage{
findFactorIntercept(beta, factorMean = NULL)
}
\arguments{
  \item{beta}{
	Regression coefficient matrix
}
  \item{factorMean}{
	Total (model-implied) factor (indicator) means. The default is that all total factor means are 0.
}
}
\value{
	A vector of factor (indicator) intercepts
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\link{findIndIntercept}} to find indicator (measurement) intercepts
		\item \code{\link{findIndMean}} to find indicator (measurement) total means
		\item \code{\link{findIndResidualVar}} to find indicator (measurement) residual variances
		\item \code{\link{findIndTotalVar}} to find indicator (measurement) total variances
		\item \code{\link{findFactorMean}} to find factor means
		\item \code{\link{findFactorResidualVar}} to find factor residual variances
		\item \code{\link{findFactorTotalVar}} to find factor total variances
		\item \code{\link{findFactorTotalCov}} to find factor covariances
	}
}
\examples{
path <- matrix(0, 9, 9)
path[4, 1] <- path[7, 4] <- 0.6
path[5, 2] <- path[8, 5] <- 0.6
path[6, 3] <- path[9, 6] <- 0.6
path[5, 1] <- path[8, 4] <- 0.4
path[6, 2] <- path[9, 5] <- 0.4
factorMean <- c(5, 2, 3, 0, 0, 0, 0, 0, 0)
findFactorIntercept(path, factorMean)
}
