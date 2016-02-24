\name{findFactorTotalVar}
\alias{findFactorTotalVar}
\title{
	Find factor total variances from regression coefficient matrix, factor (residual) correlations, and factor residual variances
}
\description{
Find factor total variances from regression coefficient matrix, factor (residual) correlation matrix, and factor residual variances  for latent variable models. In the path analysis model, this function will find indicator total variances from regression coefficient, indicator (residual) correlation matrix, and indicator residual variances. 
}
\usage{
findFactorTotalVar(beta, corPsi, residualVarPsi, gamma = NULL, covcov = NULL)
}
\arguments{
  \item{beta}{
	Regression coefficient matrix among factors
}
  \item{corPsi}{
	Factor or indicator residual correlations.
}
  \item{residualVarPsi}{
	Factor or indicator residual variances. 
}
  \item{gamma}{
	Regression coefficient matrix from covariates (column) to factors (rows)
}
  \item{covcov}{
	A covariance matrix among covariates
}
}
\value{
	A vector of factor (indicator) total variances
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
		\item \code{\link{findFactorIntercept}} to find factor intercepts
		\item \code{\link{findFactorMean}} to find factor means
		\item \code{\link{findFactorResidualVar}} to find factor residual variances
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
facCor <- diag(9)
facCor[1, 2] <- facCor[2, 1] <- 0.4
facCor[1, 3] <- facCor[3, 1] <- 0.4
facCor[2, 3] <- facCor[3, 2] <- 0.4
residualVar <- c(1, 1, 1, 0.64, 0.288, 0.288, 0.64, 0.29568, 0.21888)
findFactorTotalVar(path, facCor, residualVar)
}
