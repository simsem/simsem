\name{popDiscrepancy}
\alias{popDiscrepancy}
\title{
	Find the discrepancy value between two means and covariance matrices
}
\description{
	Find the discrepancy value between two means and covariance matrices. See the definition of each index at \code{\link{summaryMisspec}}.
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
\value{
	The discrepancy between two means and covariance matrices
}
\references{
Browne, M. W., & Cudeck, R. (1992). Alternative ways of assessing model fit. \emph{Sociological Methods & Research, 21}, 230-258.
}
\author{
    Sunthud Pornprasertmanit (Texas Tech University; \email{sunthud.pornprasertmanit@ttu.edu})
}
\examples{
m1 <- rep(0, 3)
m2 <- c(0.1, -0.1, 0.05)
S1 <- matrix(c(1, 0.6, 0.5, 0.6, 1, 0.4, 0.5, 0.4, 1), 3, 3)
S2 <- matrix(c(1, 0.55, 0.55, 0.55, 1, 0.55, 0.55, 0.55, 1), 3, 3)
popDiscrepancy(m1, S1, m2, S2)
}
