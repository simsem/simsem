\name{summaryMisspec}
\alias{summaryMisspec}
\alias{summaryMisspec-methods}
\alias{summaryMisspec,ANY-method}
\alias{summaryMisspec,SimResultParam-method}
\title{
	Provide summary of model misspecification imposed across replications
}
\description{
	This function will the amount of population misfit imposed in the real parameter
}
\usage{
summaryMisspec(object,...)
}
\arguments{
  \item{object}{
	\code{\linkS4class{SimResultParam}} object being described
}
  \item{\dots}{
    Additional arguments
}
}
\value{
	A data frame that provides the summary of model misspecification imposed on the real parameters
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}

\seealso{
	\code{\linkS4class{SimResultParam}} for the object input
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
Path.Mis.Model <- simMisspecPath(BE = mis.BE, misfitType="rmsea")

# The number of replications in actual analysis should be much more than 5
ParamObject <- simResultParam(5, Path.Model, Path.Mis.Model)

summaryMisspec(ParamObject)
}
