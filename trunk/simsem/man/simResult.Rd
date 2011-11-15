\name{simResult}
\alias{simResult}
\title{
	Create simResult.
}
\description{
This function will create simResult by different ways. One way is to create data and analyze data multiple times by specifying \code{\linkS4class{SimData}} 
and \code{\linkS4class{SimModel}} and save it in the \code{\linkS4class{SimResult}}.
}
\usage{
simResult(simData, simModel, nRep, seed = 123321, silent = FALSE)
}
\arguments{
  \item{simData}{
	Data object used in data simulation.
}
  \item{simModel}{
	Model object used in analyzing the simulated data.
}
  \item{nRep}{
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
	\code{\linkS4class{SimResult}} that saves analysis result from simulate data.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimData}} for data model specification
		\item \code{\linkS4class{SimModel}} for analysis model specification
		\item \code{\linkS4class{SimResult}} for the type of resulting object
	}
}
\examples{
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- simMatrix(loading, 0.7)
PH <- symMatrix(diag(1))
TD <- symMatrix(diag(6))
CFA.Model <- simSetCFA(LY = LX, PS = PH, TE = TD)
SimData <- simData(500, CFA.Model)
SimModel <- simModel(CFA.Model)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
Output <- simResult(SimData, SimModel, 50)
#summary(Output)
}
