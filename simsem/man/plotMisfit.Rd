\name{plotMisfit}
\alias{plotMisfit}
\title{
	Plot the population misfit in the result object
}
\description{
	Plot a histogram of the amount of population misfit in parameter result object or the scatter plot of the relationship between misspecified parameter and the population misfit or the fit indices
}
\usage{
plotMisfit(object, usedFit="default", misParam=NULL)
}
\arguments{
  \item{object}{
	The result object, \code{\linkS4class{SimResult}}
}
  \item{usedFit}{
	The sample fit indices or population misfit used to plot. All sample fit indices are available. The available population misfit are \code{"pop.f0"}, \code{"pop.rmsea"}, and \code{"pop.srmr"}. If the \code{misParam} is not specified, all population misfit are used. If the \code{misParam} is specified, the \code{"pop.rmsea"} is used in the plot.
}
  \item{misParam}{
	The index or the name of misspecified parameters used to plot.
}
}
\value{
	None. This function will plot only.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
path.BE <- matrix(0, 4, 4)
path.BE[3, 1:2] <- NA
path.BE[4, 3] <- NA
starting.BE <- matrix("", 4, 4)
starting.BE[3, 1:2] <- "runif(1, 0.3, 0.5)"
starting.BE[4, 3] <- "runif(1, 0.5, 0.7)"
mis.path.BE <- matrix(0, 4, 4)
mis.path.BE[4, 1:2] <- "runif(1, -0.1, 0.1)"
BE <- bind(path.BE, starting.BE, misspec=mis.path.BE)

residual.error <- diag(4)
residual.error[1,2] <- residual.error[2,1] <- NA
RPS <- binds(residual.error, "rnorm(1, 0.3, 0.1)")

ME <- bind(rep(NA, 4), 0)

Path.Model <- model(RPS = RPS, BE = BE, ME = ME, modelType="Path")

# The number of replications in actual analysis should be much more than 20
Output <- sim(20, n=500, Path.Model)

# Plot the distribution of population misfit
plotMisfit(Output)

# Plot the relationship between population RMSEA and all misspecified direct effects
plotMisfit(Output, misParam=1:2)

# Plot the relationship between sample CFI and all misspecified direct effects 
plotMisfit(Output, usedFit="CFI", misParam=1:2)
}
