\name{findFactorIntercept}
\alias{findFactorIntercept}
\title{
	Find factor intercept from regression coefficient matrix and factor total means
}
\description{
Find factor intercept from regression coefficient matrix and factor total means for latent variable models. In the path analysis model, this function will find indicator intercept from regression coefficient and indicator total means.
}
\usage{
findFactorIntercept(beta, factorMean = NULL, gamma = NULL, covmean = NULL)
}
\arguments{
  \item{beta}{
	Regression coefficient matrix among factors
}
  \item{factorMean}{
	A vector of total (model-implied) factor (indicator) means. The default is that all total factor means are 0.
}
  \item{gamma}{
	Regression coefficient matrix from covariates (column) to factors (rows)
}
  \item{covmean}{
	A vector of covariate means.
}
}
\value{
	A vector of factor (indicator) intercepts
}
\author{
    Sunthud Pornprasertmanit (\email{psunthud@gmail.com})
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
