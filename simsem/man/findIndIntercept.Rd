\name{findIndIntercept}
\alias{findIndIntercept}
\title{
	Find indicator intercepts from factor loading matrix, total factor mean, and indicator mean.
}
\description{
Find indicator (measurement) intercepts from a factor loading matrix, total factor mean, and indicator mean.
}
\usage{
findIndIntercept(lambda, factorMean = NULL, indicatorMean = NULL, 
	kappa = NULL, covmean = NULL)
}
\arguments{
  \item{lambda}{
	Factor loading matrix
}
  \item{factorMean}{
	Total (model-implied) mean of factors. As a default, all total factor means are 0.
}
  \item{indicatorMean}{
	Total indicator means. As a default, all total indicator means are 0.
}
  \item{kappa}{
	Regression coefficient matrix from covariates (column) to indicators (rows)
}
  \item{covmean}{
	A vector of covariate means.
}
}
\value{
	A vector of indicator (measurement) intercepts.
}
\author{
    Sunthud Pornprasertmanit (\email{psunthud@gmail.com})
}
\seealso{
	\itemize{
		\item \code{\link{findIndMean}} to find indicator (measurement) total means
		\item \code{\link{findIndResidualVar}} to find indicator (measurement) residual variances
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
facMean <- c(0.5, 0.2)
indMean <- rep(1, 6)
findIndIntercept(loading, facMean, indMean)
}
