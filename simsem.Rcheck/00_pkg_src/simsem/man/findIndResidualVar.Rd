\name{findIndResidualVar}
\alias{findIndResidualVar}
\title{
	Find indicator residual variances from factor loading matrix, total factor covariance, and total indicator variances.
}
\description{
Find indicator (measurement) residual variances from a factor loading matrix, total factor covariance matrix, and total indicator variances.
}
\usage{
findIndResidualVar(lambda, totalFactorCov, totalVarTheta = NULL)
}
\arguments{
  \item{lambda}{
	Factor loading matrix
}
  \item{totalFactorCov}{
	Total (model-implied) covariance matrix among factors. 
}
  \item{totalVarTheta}{
	Indicator total variances. As a default, all total variances are 1.
}
}
\value{
	A vector of indicator residual variances.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\link{findIndIntercept}} to find indicator (measurement) intercepts
		\item \code{\link{findIndMean}} to find indicator (measurement) total means
		\item \code{\link{findIndTotalVar}} to find indicator (measurement) total variances
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
totalVar <- rep(1, 6)
findIndResidualVar(loading, facCov, totalVar)
}
