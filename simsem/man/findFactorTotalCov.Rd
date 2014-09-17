\name{findFactorTotalCov}
\alias{findFactorTotalCov}
\title{
	Find factor total covariance from regression coefficient matrix, factor residual covariance
}
\description{
Find factor total covariances from regression coefficient matrix, factor residual covariance matrix. The residual covaraince matrix might be derived from factor residual correlation, total variance, and error variance. This function can be applied for path analysis model as well. 
}
\usage{
findFactorTotalCov(beta, psi = NULL, corPsi = NULL, totalVarPsi = NULL, 
    errorVarPsi = NULL, gamma = NULL, covcov = NULL)
}
\arguments{
  \item{beta}{
	Regression coefficient matrix among factors
}
  \item{psi}{
	Factor or indicator residual covariances. This argument can be skipped if factor residual correlation and either total variances or error variances are specified.
}
  \item{corPsi}{
	Factor or indicator residual correlation. This argument must be specified with total variances or error variances.
}
  \item{totalVarPsi}{
	Factor or indicator total variances.
}
  \item{errorVarPsi}{
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
	A matrix of factor (model-implied) total covariance
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
		\item \code{\link{findFactorMean}} to find factor means
		\item \code{\link{findFactorResidualVar}} to find factor residual variances
		\item \code{\link{findFactorTotalVar}} to find factor total variances
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
findFactorTotalCov(path, corPsi=facCor, errorVarPsi=residualVar)
}
