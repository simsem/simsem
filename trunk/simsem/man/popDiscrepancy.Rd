\name{popDiscrepancy}
\alias{popDiscrepancy}
\title{
	Find the discrepancy value between two means and covariance matrices
}
\description{
	Find the discrepancy value between two means and covariance matrices
}
\usage{
popDiscrepancy(paramM, paramCM, misspecM, misspecCM)
}
\arguments{
  \item{paramM}{
	The model-implied mean from the real parameters
}
  \item{paramCM}{
	The model-implied covariance matrix from the real parameters
}
  \item{misspecM}{
	The model-implied mean from the real and misspecified parameters
}
  \item{misspecCM}{
	The model-implied covariance matrix from the real and misspecified parameters
}
}
\details{
	The discrepancy value (\eqn{F_0}; Browne & Cudeck, 1992) is calculated by
	
	\deqn{ F_0 = tr\left( \tilde{\Sigma} \Sigma^{-1} \right) - \log{\left| \tilde{\Sigma} \Sigma^{-1} \right|} - p + \left( \tilde{\mu} - \mu \right)^{\prime} \Sigma^{-1} \left( \tilde{\mu} - \mu \right).	}
	
	where \eqn{\mu} is the model-implied mean from the real parameters, \eqn{\Sigma} is the model-implied covariance matrix from the real parameters, \eqn{\tilde{\mu}} is the model-implied mean from the real and misspecified parameters, \eqn{\tilde{\Sigma}} is the model-implied covariance matrix from the real and misspecified parameter, \emph{p} is the number of indicators. 
}
\value{
	The discrepancy between two means and covariance matrices
}
\references{
Browne, M. W., & Cudeck, R. (1992). Alternative ways of assessing model fit. \emph{Sociological Methods & Research, 21}, 230-258.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
m1 <- rep(0, 3)
m2 <- c(0.1, -0.1, 0.05)
S1 <- matrix(c(1, 0.6, 0.5, 0.6, 1, 0.4, 0.5, 0.4, 1), 3, 3)
S2 <- matrix(c(1, 0.55, 0.55, 0.55, 1, 0.55, 0.55, 0.55, 1), 3, 3)
popDiscrepancy(m1, S1, m2, S2)
}
