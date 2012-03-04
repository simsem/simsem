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
simResult(nRep, simData, simModel, simMissing = new("NullSimMissing"), seed = 123321, silent = FALSE, multicore = FALSE, cluster = FALSE, numProc=NULL)
}
\arguments{
  \item{nRep}{
	Number of replications.
}
  \item{simData}{
	Data object used in data simulation.
}
  \item{simModel}{
	Model object used in analyzing the simulated data.
}
  \item{simMissing}{
	Model object used in providing the information about missing values.
}
  \item{seed}{
	Seed number
}
  \item{silent}{
	TRUE if users do not wish to print number of replications during running the function.
}
  \item{multicore}{
	Use multiple processors within a computer. Specify as TRUE to use it.
}
  \item{cluster}{
	Not applicable now. Use for specify nodes in hpc in order to be parallelizable.
}
  \item{numProc}{
	Number of processors for using multiple processors. If it is \code{NULL}, the package will find the maximum number of processors.
}
}
\value{
	\code{\linkS4class{SimResult}} that saves analysis result from simulate data.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu}); 
	Alex Schoemann (University of Kansas; \email{schoemann@ku.edu}); 
	Patrick Miller (University of Kansas; \email{patr1ckm@ku.edu})
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
RPH <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model <- simSetCFA(LY = LX, RPS = RPH, RTE = RTD)
SimData <- simData(500, CFA.Model)
SimModel <- simModel(CFA.Model)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
Output <- simResult(50, SimData, SimModel)
#summary(Output)
}
