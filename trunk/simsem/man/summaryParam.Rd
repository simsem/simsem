\name{summaryParam}
\alias{summaryParam}
\alias{summaryParam-methods}
\alias{summaryParam,ANY-method}
\alias{summaryParam,SimResult-method}
\title{
	Provide summary of parameter estimates and standard error across replications
}
\description{
	This function will provide averages of parameter estimates, standard deviations of parameter estimates, averages of standard errors, and power of rejection with a priori alpha level for the null hypothesis of parameters equal 0.
}
\usage{
summaryParam(object, ...)
}
\arguments{
  \item{object}{
	\code{\linkS4class{SimResult}} object being described
}
  \item{\dots}{
    any additional arguments
}
}
\value{
	A data frame that provides the statistics described above from all parameters.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\code{\linkS4class{SimResult}} for the object input
}
\examples{
showClass("SimResult")
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
summaryParam(Output)
}
