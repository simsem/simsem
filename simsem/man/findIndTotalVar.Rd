\name{findIndTotalVar}
\alias{findIndTotalVar}
\title{
	Find indicator total variances from factor loading matrix, total factor covariance, and indicator residual variances.
}
\description{
Find indicator total variances from a factor loading matrix, total factor covariance matrix, and indicator (measurement) residual variances.
}
\usage{
findIndTotalVar(lambda, totalFactorCov, residualVarTheta, kappa = NULL, 
	covcov = NULL)
}
\arguments{
  \item{lambda}{
	Factor loading matrix
}
  \item{totalFactorCov}{
	Total (model-implied) covariance matrix among factors. 
}
  \item{residualVarTheta}{
	Indicator (measurement) residual variances. 
}
  \item{kappa}{
	Regression coefficient matrix from covariates (column) to indicators (rows)
}
  \item{covcov}{
	A covariance matrix among covariates
}
}
\value{
	A vector of indicator total variances.
}
\author{
    Sunthud Pornprasertmanit (\email{psunthud@gmail.com})
}
\seealso{
	\itemize{
		\item \code{\link{findIndIntercept}} to find indicator (measurement) intercepts
		\item \code{\link{findIndMean}} to find indicator (measurement) total means
		\item \code{\link{findIndResidualVar}} to find indicator (measurement) residual variances
		\item \code{\link{findFactorIntercept}} to find factor intercepts
		\item \code{\link{findFactorMean}} to find factor means
		\item \code{\link{findFactorResidualVar}} to find factor residual variances
		\item \code{\link{findFactorTotalVar}} to find factor total variances
		\item \code{\link{findFactorTotalCov}} to find factor covariances
	}
}
\examples{
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- c(0.6, 0.7, 0.8)
loading[4:6, 2] <- c(0.6, 0.7, 0.8)
facCov <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
resVar <- c(0.64, 0.51, 0.36, 0.64, 0.51, 0.36)
findIndTotalVar(loading, facCov, resVar)
}
