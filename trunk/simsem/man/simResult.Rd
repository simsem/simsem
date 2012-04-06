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
simResult(nRep = NULL, objData = NULL, objModel = NULL, objMissing = new("NullSimMissing"), seed = 123321, silent = FALSE, multicore = FALSE, cluster = FALSE, numProc=NULL, n=NULL, pmMCAR=NULL, pmMAR=NULL, objSet=NULL)
}
\arguments{
  \item{nRep}{
	Number of replications. Users can specify as \code{NULL} and specify \code{n}, \code{pmMCAR}, and \code{pmMAR} as a vector instead. By this, the number of replications will be calculated from the length of \code{n}, \code{pmMCAR}, and \code{pmMAR}.
}
  \item{objData}{
	Data object used in data simulation.
}
  \item{objModel}{
	Model object used in analyzing the simulated data.
}
  \item{objMissing}{
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
  \item{n}{
	Sample size. This argument is not necessary except the user wish to vary sample size across replications. The sample size here can be random distribution object (\code{\linkS4class{VirtualDist}}), or a vector of sample size in integers. For the random distribution object, if the resulting value has decimal, the value will be rounded. 
}
  \item{pmMCAR}{
	The percent completely missing at random. This argument is not necessary except the user wish to vary percent missing completely at random across replications. The \code{pmMCAR} here can be random distribution object (\code{\linkS4class{VirtualDist}}), or a vector of percent missing, which the values can be in between 0 and 1 only. The specification of \code{objMissing} is not needed (but is needed if users wish to specify complex missing value data generation or wish to use multiple imputation). 
}
  \item{pmMAR}{
	The percent missing at random. This argument is not necessary except the user wish to vary percent missing at random across replications. The \code{pmMAR} here can be random distribution object (\code{\linkS4class{VirtualDist}}), or a vector of percent missing, which the values can be in between 0 and 1 only. The specification of \code{objMissing} is not needed (but is needed if users wish to specify complex missing value data generation or wish to use multiple imputation). 
}
  \item{objSet}{
	The \code{\linkS4class{SimSet}} object for data generation and analysis model. Users can specify this argument directly and not specify \code{objData} and \code{objModel}
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
summary(Output)

# Specify Sample Size by n
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
Output <- simResult(NULL, SimData, SimModel, n=seq(50, 500, 10))
summary(Output)

# Specify both sample size and percent missing completely at random
Output <- simResult(NULL, SimData, SimModel, n=seq(50, 500, 25), pmMCAR=c(0, 0.1, 0.2))
summary(Output)

# Use distribution object on sample size and percent completely at random
n <- simUnif(100, 500)
pmMCAR <- simUnif(0, 0.1)
Output <- simResult(50, SimData, SimModel, n=n, pmMCAR=pmMCAR)

}
