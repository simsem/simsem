\name{simDataDist}
\alias{simDataDist}
\title{
	Create a data distribution object.
}
\description{
This function will create simResult by different ways. One way is to create data and analyze data multiple times by specifying \code{\linkS4class{SimData}} 
and \code{\linkS4class{SimModel}} and save it in the \code{\linkS4class{SimDataDist}}.
}
\usage{
simDataDist(..., p=NULL, keepScale=TRUE, reverse=FALSE)
}
\arguments{
  \item{...}{
	List of distribution objects. See \code{\linkS4class{VirtualDist}} for a list of possible distributions. 
}
  \item{p}{
	Number of variables. If only one distribution object is listed, the \code{p} will make the same distribution objects for all variables.
}
  \item{keepScale}{
	If TRUE, transform back to retain the mean and standard deviation of a variable equal to the model implied mean and standard deviation (with sampling error)
}
  \item{reverse}{
	A vector representing of mirrorring each variable or not. If \code{TRUE}, reverse the distribution of a variable (e.g., from positive skewed to negative skewed. If one logical value is specified, it will apply to all variables.
}
}
\value{
	\code{\linkS4class{SimDataDist}} that saves analysis result from simulate data.
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
RPH <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model <- simSetCFA(LY = LX, RPS = RPH, RTE = RTD)
SimData <- simData(CFA.Model, 500)
SimModel <- simModel(CFA.Model)
# We make the examples running only 5 replications to save time.
# In reality, more replications are needed.
Output <- simResult(5, SimData, SimModel)
#summary(Output)
}
