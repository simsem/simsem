\name{result.object}
\alias{result.object}
\title{
	Create result object.
}
\description{
This function will create result object by different ways. One way is to create data and analyze data multiple times by specifying \code{\linkS4class{simData}} 
and \code{\linkS4class{simModel}} and save it in the \code{\linkS4class{simResult}}.
}
\usage{
result.object(simData, simModel, NRep, seed = 123321, silent = FALSE)
}
\arguments{
  \item{simData}{
	Data object used in data simulation.
}
  \item{simModel}{
	Model object used in analyzing the simulated data.
}
  \item{NRep}{
	Number of replications.
}
  \item{seed}{
	Seed number
}
  \item{silent}{
	TRUE if users do not wish to print number of replications during running the function.
}
}
\value{
	\code{\linkS4class{simResult}} that saves analysis result from simulate data.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\enumerate{
		\item \code{\linkS4class{simData}} for data model specification
		\item \code{\linkS4class{simModel}} for analysis model specification
		\item \code{\linkS4class{simResult}} for the type of resulting object
	}
}
\examples{
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- matrix.object(loading, 0.7)
PH <- sym.matrix.object(diag(1))
TD <- sym.matrix.object(diag(6))
CFA.Model <- matrix.CFA.object(LY = LX, PS = PH, TE = TD)
SimData <- data.object(500, CFA.Model)
SimModel <- model.object(CFA.Model)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
Output <- result.object(SimData, SimModel, 50)
#summary(Output)
}
