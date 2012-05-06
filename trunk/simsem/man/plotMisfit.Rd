\name{plotMisfit}
\alias{plotMisfit}
\title{
	Plot the population misfit in parameter result object
}
\description{
	Plot a histogram of the amount of population misfit in parameter result object or the scatter plot of the relationship between misspecified parameter and the population misfit
}
\usage{
plotMisfit(object, usedFit="default", misParam=NULL)
}
\arguments{
  \item{object}{
	The parameter result object, \code{\linkS4class{SimResultParam}}
}
  \item{usedFit}{
	The fit indices used to plot. If the \code{misParam} is not specified, all fit indices are used. If the \code{misParam} is specified, the \code{"rmsea"} is used in the plot.
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
u35 <- simUnif(0.3, 0.5)
u57 <- simUnif(0.5, 0.7)
u1 <- simUnif(-0.1, 0.1)
n31 <- simNorm(0.3, 0.1)

path.BE <- matrix(0, 4, 4)
path.BE[3, 1:2] <- NA
path.BE[4, 3] <- NA
starting.BE <- matrix("", 4, 4)
starting.BE[3, 1:2] <- "u35"
starting.BE[4, 3] <- "u57"
BE <- simMatrix(path.BE, starting.BE)

residual.error <- diag(4)
residual.error[1,2] <- residual.error[2,1] <- NA
RPS <- symMatrix(residual.error, "n31")

ME <- simVector(rep(NA, 4), 0)

Path.Model <- simSetPath(RPS = RPS, BE = BE, ME = ME)

mis.path.BE <- matrix(0, 4, 4)
mis.path.BE[4, 1:2] <- NA
mis.BE <- simMatrix(mis.path.BE, "u1")
Path.Mis.Model <- simMisspecPath(BE = mis.BE, misfitType="rmsea") #, misfitBound=c(0.05, 0.08))

# The number of replications in actual analysis should be much more than 50
ParamObject <- simResultParam(50, Path.Model, Path.Mis.Model)
plotMisfit(ParamObject)

plotMisfit(ParamObject, misParam=1:2)
}
