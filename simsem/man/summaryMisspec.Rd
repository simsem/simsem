\name{summaryMisspec}
\alias{summaryMisspec}
\title{
Provide summary of the population misfit and misspecified-parameter values across replications
}
\description{
This function provides the summary of the population misfit and misspecified-parameter values across replications. The summary will be provided for the convergent replications only.
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
A data frame that provides the summary of population misfit and misspecified-parameter values imposed on the real parameters
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
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
