\name{runFit}
\alias{runFit}
\alias{runFit-methods}
\alias{runFit,ANY-method}
\alias{runFit,SimModel-method}
\alias{runFit,SimModelOut-method}
\title{
	Build a Monte Carlo simulation that the data-generation parameters are from the result of analyzing real data
}
\description{
This function will analyze real data and use the result of the analysis to simulate data. Those simulated data will be analyzed by the same model. The result of the analyses will be saved in the result object, \code{\linkS4class{SimResult}}. 
}
\usage{
runFit(model, data, nRep=1000, misspec=new("NullSimMisspec"), 
	maxDraw=100, sequential=NA, facDist=new("NullSimDataDist"), 
	errorDist=new("NullSimDataDist"), indDist=new("NullSimDataDist"), 
	modelBoot=FALSE, seed=123321, silent=FALSE, multicore=FALSE, 
	cluster=FALSE, numProc=NULL, empiricalMissing=TRUE, 
	missModel=new("NullSimMissing"), usedStd=TRUE)
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
  \item{modelBoot}{
	Use a model-based bootstrap approach to create data. See \code{\link{simData}} for further details.
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
  \item{empiricalMissing}{
	Use the missing pattern from the real data imposing on the simulated data if \code{empiricalMissing=TRUE}. If \code{FALSE}, the missing pattern will be generated from the missing object specified in the \code{missModel} argument. If we need a complete data, the \code{missModel} argument can be specified as blank missing object, \code{simMissing()}.
}
  \item{missModel}{
	A missing object used for data simulation. Only \code{numImps} attribute within the missing object is used if \code{empiricalMissing=TRUE}. All attributes are used (for generating missing data) if \code{empiricalMissing=FALSE}.
}
  \item{usedStd}{
	The standardized parameters are used for data generation if \code{usedStd=TRUE}. If \code{usedStd=FALSE}, unstandardized parameters are used.
}
}
\details{
	This function can be used to implement the Monte Carlo approach for evaluating model fit proposed by Millsap (in press). This function will use the obtained parameter estimates as the real population parameters in a simulation study, put a trivial model misspecification in the real parameters, generate multiple simulated data, analyze those simulated data, and put it in the result object, \code{\linkS4class{SimResult}}. The fit indices distribution in the result object can be used for the model fit evaluation. The useful functions for model fit evaluation are \code{\link{plotCutoff}}, \code{\link{getCutoff}}, and \code{\link{pValue}}. If \code{modelBoot} is \code{TRUE}, the function will use the Bollen-Stine bootstrap method for data generation. See \code{\link{simData}} for further details of the Bollen-Stine bootstrap approach.
}
\value{
	\code{\linkS4class{SimResult}} that saves analysis results from simulated data.
}
\references{
	\itemize{
		\item Millsap, R.E. (in press). A simulation paradigm for evaluating model fit. In M. Edwards and R. MacCallum (Eds.) \emph{Current Issues in the Theory and Application of Latent Variable Models.} New York: Routledge.
	}
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
Output <- runFit(SimModel, HolzingerSwineford1939, 5, mis)
summary(Output)

out <- run(SimModel, HolzingerSwineford1939)
Output2 <- runFit(out, HolzingerSwineford1939, 5, mis)

# Bollen-Stine Bootstrap
Output3 <- runFit(out, HolzingerSwineford1939, 5, modelBoot=TRUE)

# Bollen-Stine Bootstrap with trivial misspecification
Output4 <- runFit(out, HolzingerSwineford1939, 5, mis, modelBoot=TRUE)

# Example with multiple imputation
library(lavaan)
loading <- matrix(0, 11, 3)
loading[1:3, 1] <- NA
loading[4:7, 2] <- NA
loading[8:11, 3] <- NA
path <- matrix(0, 3, 3)
path[2:3, 1] <- NA
path[3, 2] <- NA
errorCov <- diag(NA, 11)
facCov <- diag(3)
param <- simParamSEM(LY=loading, BE=path, TE=errorCov, PS=facCov)

miss <- simMissing(pmMCAR=0.03, numImps=5)
usedData <- run(miss, PoliticalDemocracy)

model <- simModel(param, indLab=c(paste("x", 1:3, sep=""), paste("y", 1:8, sep="")))
out <- run(model, usedData, miss)
output <- runFit(model, usedData, 5, missModel=miss)
pValue(out, output)
}
