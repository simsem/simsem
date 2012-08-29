\name{summaryMisspec}
\alias{summaryMisspec}
\alias{summaryMisspec-methods}
\alias{summaryMisspec,ANY-method}
%\alias{summaryMisspec,SimResult-method}
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
	TBA %\code{\linkS4class{SimResultParam}} object being described
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
	\code{\linkS4class{SimResult}} for the object input
}
\examples{
# Incomplete

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


# The number of replications in actual analysis should be much more than 5
# ParamObject <- simResultParam(5, Path.Model, Path.Mis.Model)

# summaryMisspec(ParamObject)
}
