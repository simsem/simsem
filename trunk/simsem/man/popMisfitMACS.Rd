\name{popMisfitMACS}
\alias{popMisfitMACS}
\title{
	Find population misfit by sufficient statistics
}
\description{
	Find the value quantifying the amount of population misfit: \eqn{F_0}, RMSEA, and SRMR.
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
\details{
	The root mean squared error of approximation (RMSEA) is calculated by
	
	\deqn{RMSEA = \sqrt{\frac{F_0}{df}}}
	
	where \eqn{F_0} is the discrepancy value between two means vectors and covariance matrices (see \code{\link{popDiscrepancy}}) and \eqn{df} is the degree of freedom in the real model.
	
	The standardized root mean squared residual can be calculated by
	
	\deqn{SRMR = \sqrt{\frac{2\sum_{i} \sum_{j \le i} \left( \frac{s_{ij}}{\sqrt{s_{ii}}\sqrt{s_{jj}}} - \frac{\hat{sigma}_{ij}}{\sqrt{\hat{sigma}_{ii}}\sqrt{\hat{sigma}_{jj}}} \right)}{p(p + 1)}}}
	
	where \eqn{s_{ij}} is the observed covariance between indicators \emph{i} and \emph{j}, \eqn{\hat{\sigma}_{ij}} is the model-implied covariance between indicators \emph{i} and \emph{j}, \emph{p} is the number of indicators.
}
\value{
	The vector of the misfit indices
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
popMisfitMACS(m1, S1, m2, S2)
}
