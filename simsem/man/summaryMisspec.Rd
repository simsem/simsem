\name{summaryMisspec}
\alias{summaryMisspec}
\title{
Provide summary of the population misfit and misspecified-parameter values across replications
}
\description{
This function provides the summary of the population misfit and misspecified-parameter values across replications. The summary will be summarized for the convergent replications only.
}
\usage{
summaryMisspec(object)
}
\arguments{
  \item{object}{
		\code{\linkS4class{SimResult}} object being described
}
}
\value{
	A data frame that provides the summary of population misfit and misspecified-parameter values imposed on the real parameters.

	The discrepancy value (\eqn{f_0}; Browne & Cudeck, 1992) is calculated by
	
	\deqn{ F_0 = tr\left( \tilde{\Sigma} \Sigma^{-1} \right) - \log{\left| \tilde{\Sigma} \Sigma^{-1} \right|} - p + \left( \tilde{\mu} - \mu \right)^{\prime} \Sigma^{-1} \left( \tilde{\mu} - \mu \right).	}
	
	where \eqn{\mu} is the model-implied mean from the real parameters, \eqn{\Sigma} is the model-implied covariance matrix from the real parameters, \eqn{\tilde{\mu}} is the model-implied mean from the real and misspecified parameters, \eqn{\tilde{\Sigma}} is the model-implied covariance matrix from the real and misspecified parameter, \emph{p} is the number of indicators. For the multiple groups, the \eqn{f_0} value is the sum of this value across groups.
	
	The root mean squared error of approximation (rmsea) is calculated by
	
	\deqn{rmsea = \sqrt{\frac{f_0}{df}}}
	
	where \eqn{f_0} is the discrepancy value between two means vectors and covariance matrices (see \code{\link{popDiscrepancy}}) and \eqn{df} is the degree of freedom in the real model.
	
	The standardized root mean squared residual (srmr) can be calculated by
	
	\deqn{srmr = \sqrt{\frac{2\sum_{g} \sum_{i} \sum_{j \le i} \left( \frac{s_{gij}}{\sqrt{s_{gii}}\sqrt{s_{gjj}}} - \frac{\hat{\sigma}_{gij}}{\sqrt{\hat{\sigma}_{gii}}\sqrt{\hat{\sigma}_{gjj}}} \right)}{g \times p(p + 1)}}}
	
	where \eqn{s_{gij}} is the observed covariance between indicators \emph{i} and \emph{j} in group \emph{g}, \eqn{\hat{\sigma}_{ij}} is the model-implied covariance between indicators \emph{i} and \emph{j} in group \emph{g}, \emph{p} is the number of indicators.

}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\references{
Browne, M. W., & Cudeck, R. (1992). Alternative ways of assessing model fit. \emph{Sociological Methods & Research, 21}, 230-258.
}
\seealso{
	\code{\linkS4class{SimResult}} for the object input
}
\examples{
\dontrun{
path <- matrix(0, 4, 4)
path[3, 1:2] <- NA
path[4, 3] <- NA
pathVal <- matrix("", 4, 4)
pathVal[3, 1:2] <- "runif(1, 0.3, 0.5)"
pathVal[4, 3] <- "runif(1, 0.5, 0.7)"
pathMis <- matrix(0, 4, 4)
pathMis[4, 1:2] <- "runif(1, -0.1, 0.1)"
BE <- bind(path, pathVal, pathMis)

residual.error <- diag(4)
residual.error[1,2] <- residual.error[2,1] <- NA
RPS <- binds(residual.error, "rnorm(1, 0.3, 0.1)")

Path.Model <- model(RPS = RPS, BE = BE, modelType="Path")

# The number of replications in actual analysis should be much more than 5
ParamObject <- sim(5, n=200, Path.Model)

summaryMisspec(ParamObject)
}
}
