\name{runFitParam}
\alias{runFitParam}
\alias{runFitParam-methods}
\alias{runFitParam,ANY-method}
\alias{runFitParam,SimModel-method}
\alias{runFitParam,SimModelOut-method}
\title{
	Build a parameter result object that the data-generation parameters are from the result of analyzing real data
}
\description{
This function will analyze real data and use the result of the analysis to find the range of parameters used in data simulation. 
}
\usage{
runFitParam(model, nRep = 1000, misspec = new("NullSimMisspec"), 
maxDraw = 100, seed = 123321, usedStd = TRUE, ...)
}
\arguments{
  \item{model}{
	Model object used in analyzing the real and simulated data.
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
  \item{seed}{
	Seed number
}
  \item{usedStd}{
	The standardized parameters are used for data generation if \code{usedStd=TRUE}. If \code{usedStd=FALSE}, unstandardized parameters are used.
}
  \item{\dots}{
	The only additional argument is \code{data}, which is the real data that will be used in the analysis. The data argument is used when the \code{model} is \code{linkS4class{SimModel}}
}
}
\details{
	This function will use the obtained parameter estimates as the real population parameters in a simulation study, put a trivial model misspecification in the real parameters, and report the real and misspecified parameters in the \code{SimResultParam} object. 
}
\value{
	\code{\linkS4class{SimResultParam}} that saves the parameters and misspecification used in each replication.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimModel}} for analysis model specification
		\item \code{\linkS4class{SimResult}} for the type of resulting object
		\item \code{\link{runFit}} for data simulation based on real data
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
Output <- runFitParam(SimModel, data=HolzingerSwineford1939, nRep=5, misspec=mis)
summary(Output)

out <- run(SimModel, HolzingerSwineford1939)
Output2 <- runFitParam(out, nRep=5, misspec=mis)
}
