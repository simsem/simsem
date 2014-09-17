\name{popMisfitMACS}
\alias{popMisfitMACS}
\title{
	Find population misfit by sufficient statistics
}
\description{
	Find the value quantifying the amount of population misfit: \eqn{F_0}, RMSEA, and SRMR. See the definition of each index at \code{\link{summaryMisspec}}.
}
\usage{
popMisfitMACS(paramM, paramCM, misspecM, misspecCM, dfParam=NULL, fit.measures="all")
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
  \item{dfParam}{
	The degree of freedom of the real model
}
  \item{fit.measures}{
	The names of indices used to calculate population misfit. There are three types of misfit: 1) discrepancy function (\code{"f0"}; see \code{\link{popDiscrepancy}}), 2) root mean squared error of approximation (\code{"rmsea"}; Equation 12 in Browne & Cudeck, 1992), and 3) standardized root mean squared residual (\code{"srmr"})
}
}
\value{
	The vector of the misfit indices
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
popMisfitMACS(m1, S1, m2, S2)
}
