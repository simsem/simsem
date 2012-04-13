\name{runFit}
\alias{runFit}
\alias{runFit-methods}
\alias{runFit,ANY-method}
\alias{runFit,SimModel-method}
\alias{runFit,SimModelOut-method}
\title{
	Build a result object from analyzing real data
}
\description{
This function will analyze real data and use the result of the analysis to simulate data. Those simulated data will be analyzed by the same model. The result of the analyses will be saved in the result object, \code{\linkS4class{SimResult}}.
}
\usage{
runFit(model, data, nRep=1000, misspec=new("NullSimMisspec"), conBeforeMis=TRUE, misfitBound=new("NullVector"), maxDraw=100, sequential=NA, facDist=new("NullSimDataDist"), errorDist=new("NullSimDataDist"), indDist=new("NullSimDataDist"), seed=123321, silent=FALSE, multicore=FALSE, cluster=FALSE, numProc=NULL)
}
\arguments{
  \item{model}{
	Model object used in analyzing the real and simulated data.
}
  \item{data}{
	Real data that will be used in the analysis
}
  \item{nRep}{
	Number of replications.
}
  \item{misspec}{
	Model \emph{misspecification} matrices that are created by \code{\link{simMisspecCFA}}, \code{\link{simMisspecPath}}, or \code{\link{simMisspecSEM}}.
}
  \item{conBeforeMis}{
	\code{TRUE} if users wish to constrain parameters before adding misspecification. \code{FALSE} if users wish to constrain parameters after adding misspecification.
}
  \item{misfitBound}{
	Upper bound of population root mean squared error of approximation (RMSEA; Browne & Cudeck, 1992; Steiger & Lind, 1980) that users wish their model misspecification to be
}
  \item{maxDraw}{
	The maximum number of random drawn parameters and misspecification model until all parameters in the model are eligible (no negative error variance, standardized coefficients over 1).
}
  \item{sequential}{
	If \code{TRUE}, use a sequential method to create data such that the data from factor are generated first and apply to a set of equations to obtain the data of indicators. If \code{FALSE}, create data directly from model-implied mean and covariance of indicators.
}
  \item{facDist}{
	A \code{\linkS4class{SimDataDist}} for a distribution of factors. Use when \code{sequential} is \code{TRUE}.
}
  \item{errorDist}{
	A \code{\linkS4class{SimDataDist}} for a distribution of measurement errors. Use when \code{sequential} is \code{TRUE}
}
  \item{indDist}{
	A \code{\linkS4class{SimDataDist}} for a distribution of indicators. Use when \code{sequential} is \code{FALSE}.
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
	\code{\linkS4class{SimResult}} that saves analysis results from simulated data.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimModel}} for analysis model specification
		\item \code{\linkS4class{SimResult}} for the type of resulting object
	}
}
\examples{
library(lavaan)
loading <- matrix(0, 9, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
model <- simParamCFA(LY=loading)
SimModel <- simModel(model, indLab=paste("x", 1:9, sep=""))
u2 <- simUnif(-0.2, 0.2)
loading.trivial <- matrix(NA, 9, 3)
loading.trivial[is.na(loading)] <- 0
LY.trivial <- simMatrix(loading.trivial, "u2")
mis <- simMisspecCFA(LY = LY.trivial)
Output <- runFit(SimModel, HolzingerSwineford1939, 20, mis)
summary(Output)

out <- run(SimModel, HolzingerSwineford1939)
Output2 <- runFit(out, HolzingerSwineford1939, 20, mis)
}
