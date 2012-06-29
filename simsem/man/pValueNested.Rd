\name{pValueNested}
\alias{pValueNested}
\title{
	Find p-values (1 - percentile) for a nested model comparison
}
\description{
	This function will provide \emph{p} value from comparing the differences in fit indices between nested models with the simulation results of both parent and nested models when the nested model is true. 
}
\usage{
pValueNested(outNested, outParent, simNested, simParent, usedFit = NULL, 
nVal = NULL, pmMCARval = NULL, pmMARval = NULL, df = 0)
}
\arguments{
  \item{outNested}{
	\code{\linkS4class{SimModelOut}} that saves the analysis result of the nested model from the target dataset
}
  \item{outParent}{
	\code{\linkS4class{SimModelOut}} that saves the analysis result of the parent model from the target dataset
}
  \item{simNested}{
	\code{\linkS4class{SimResult}} that saves the analysis results of nested model from multiple replications
}
  \item{simParent}{
	\code{\linkS4class{SimResult}} that saves the analysis results of parent model from multiple replications
}
  \item{usedFit}{
	Vector of names of fit indices that researchers wish to getCutoffs from. The default is to getCutoffs of all fit indices.
}
  \item{nVal}{
	The sample size value that researchers wish to find the \emph{p} value from.
}
  \item{pmMCARval}{
	The percent missing completely at random value that researchers wish to find the \emph{p} value from.
}
  \item{pmMARval}{
	The percent missing at random value that researchers wish to find the the \emph{p} value from.
}
  \item{df}{
	The degree of freedom used in spline method in predicting the fit indices by the predictors. If \code{df} is 0, the spline method will not be applied.
}
}
\value{
	This function provides a vector of \emph{p} values based on the comparison of the difference in fit indices from the real data with the simulation result. The \emph{p} values of fit indices are provided, as well as two additional values: \code{andRule} and \code{orRule}. The \code{andRule} is based on the principle that the model is retained only when all fit indices provide good fit. The proportion is calculated from the number of replications that have all fit indices indicating a better model than the observed data. The proportion from the \code{andRule} is the most stringent rule in retaining a hypothesized model. The \code{orRule} is based on the principle that the model is retained only when at least one fit index provides good fit. The proportion is calculated from the number of replications that have at least one fit index indicating a better model than the observed data. The proportion from the \code{orRule} is the most lenient rule in retaining a hypothesized model.
}
\details{
	In comparing fit indices, the \emph{p} value is the proportion of the number of replications that provide less preference for nested model (e.g., larger negative difference in CFI values or larger positive difference in RMSEA values) than the analysis result from the observed data. 
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimModelOut}} to see how to get the analysis result of observed data
		\item \code{\linkS4class{SimResult}} to run a simulation study
		\item \code{\link{runFit}} to run a simulation study based on the parameter estimates from the analysis result of observed data
	}
}
\examples{
library(lavaan)

LY <- matrix(1, 4, 2)
LY[,2] <- 0:3
PS <- matrix(NA, 2, 2)
TY <- rep(0, 4)
AL <- rep(NA, 2)
TE <- diag(NA, 4)
linearModel <- simParamCFA(LY=LY, PS=PS, TY=TY, AL=AL, TE=TE)

LY2 <- matrix(1, 4, 2)
LY2[,2] <- c(0, NA, NA, 3)
unconstrainModel <- simParamCFA(LY=LY2, PS=PS, TY=TY, AL=AL, TE=TE)

nested <- simModel(linearModel, indLab=paste("t", 1:4, sep=""))
parent <- simModel(unconstrainModel, indLab=paste("t", 1:4, sep=""))

outNested <- run(nested, Demo.growth)
outParent <- run(parent, Demo.growth)

loadingMis <- matrix(0, 4, 2)
loadingMis[2:3, 2] <- NA
LYmis <- simMatrix(loadingMis, "runif(1, -0.1, 0.1)")
linearMis <- simMisspecCFA(LY=LYmis)

simNestedNested <- runFit(model=nested, data=Demo.growth, nRep=10, misspec=linearMis)
simNestedParent <- runFit(model=nested, data=Demo.growth, nRep=10, misspec=linearMis, analyzeModel=parent)

pValueNested(outNested, outParent, simNestedNested, simNestedParent)
}
