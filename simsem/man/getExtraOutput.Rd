\name{getExtraOutput}
\alias{getExtraOutput}
\title{
	Get extra outputs from the result of simulation
}
\description{
	Get extra outputs from a simulation result object (\code{\linkS4class{SimResult}}). Users can ask this package to extra output from the \code{\linkS4class{lavaan}} object in each iteration by setting the \code{outfun} argument (in the \code{sim} function). See the example below.
}
\usage{
getExtraOutput(object)
}
\arguments{
  \item{object}{
	\code{\linkS4class{SimResult}} that have the extra output extracted by the function defined in the \code{outfun} argument (in the \code{sim} function)
}
}
\value{
	A list of extra outputs
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\link{sim}} A function to run a Monte Carlo simulation
	}
}
\examples{
\dontrun{
# Specify Sample Size by n
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- bind(loading, 0.7)
RPH <- binds(diag(1))
RTD <- binds(diag(6))
CFA.Model <- model(LY = LX, RPS = RPH, RTE = RTD, modelType="CFA")

# We will use only 5 replications to save time.
# In reality, more replications are needed.

outfun <- function(out) {
	result <- inspect(out, "mi")
}

# Specify both sample size and percent missing completely at random
Output <- sim(5, n=200, model=CFA.Model, outfun=outfun)
getExtraOutput(Output)
}
}
