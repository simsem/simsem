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
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LY <- bind(loading, 0.7)
RPS <- binds(diag(1))
RTE <- binds(diag(6))
CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA")

# Write a function to extract the modification index from lavaan object
outfun <- function(out) {
	result <- inspect(out, "mi")
}

# We will use only 5 replications to save time.
# In reality, more replications are needed.
Output <- sim(5, n=200, model=CFA.Model, outfun=outfun)

# Get the modification index of each replication
getExtraOutput(Output)
}
}
