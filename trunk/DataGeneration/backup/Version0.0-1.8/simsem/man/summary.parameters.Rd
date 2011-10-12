\name{summary.parameters}
\alias{summary.parameters}
\alias{summary.parameters-methods}
\alias{summary.parameters,ANY-method}
\alias{summary.parameters,simResult-method}
\title{
	Provide summary of parameter estimates and standard error across replications
}
\description{
	This function will provide averages of parameter estimates, standard deviations of parameter estimates, averages of standard errors, and power of rejection with a priori alpha level for the null hypothesis of parameters equal 0.
}
\usage{
summary.parameters(object, ...)
}
\arguments{
  \item{object}{
	\code{\linkS4class{simResult}} object being described
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
	\enumerate{
		\item \code{\linkS4class{simResult}} for the object input
	}
}
\examples{
showClass("simResult")
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
summary.parameters(Output)
}
