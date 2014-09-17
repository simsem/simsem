\name{findFactorMean}
\alias{findFactorMean}
\title{
	Find factor total means from regression coefficient matrix and factor intercept
}
\description{
Find factor total means from regression coefficient matrix and factor intercepts for latent variable models. In the path analysis model, this function will find indicator total means from regression coefficient and indicator intercept.
}
\usage{
findFactorMean(beta, alpha = NULL, gamma = NULL, covmean = NULL)
}
\arguments{
  \item{beta}{
	Regression coefficient matrix among factors
}
  \item{alpha}{
	Factor (indicator) intercept. The default is that all factor intercepts are 0.
}
  \item{gamma}{
	Regression coefficient matrix from covariates (column) to factors (rows)
}
  \item{covmean}{
	A vector of covariate means.
}
}
\value{
	A vector of factor (indicator) total means
}
\author{
    Sunthud Pornprasertmanit (Texas Tech University; \email{sunthud.pornprasertmanit@ttu.edu})
}
\seealso{
	\itemize{
		\item \code{\link{findIndIntercept}} to find indicator (measurement) intercepts
		\item \code{\link{findIndMean}} to find indicator (measurement) total means
		\item \code{\link{findIndResidualVar}} to find indicator (measurement) residual variances
		\item \code{\link{findIndTotalVar}} to find indicator (measurement) total variances
		\item \code{\link{findFactorIntercept}} to find factor intercepts
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
intcept <- c(5, 2, 3, 0, 0, 0, 0, 0, 0)
findFactorMean(path, intcept)
}
