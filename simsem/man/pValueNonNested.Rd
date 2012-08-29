\name{pValueNonNested}
\alias{pValueNonNested}
\title{
	Find p-values (1 - percentile) for a non-nested model comparison
}
\description{
	This function will provide \emph{p} value from comparing the results of fitting real data into two models against the simulation from fitting the simulated data from both models into both models. The \emph{p} values from both sampling distribution under the datasets from the first and the second models are reported.
}
\usage{
pValueNonNested(outMod1, outMod2, dat1Mod1, dat1Mod2, dat2Mod1, dat2Mod2, 
usedFit = NULL, nVal = NULL, pmMCARval = NULL, pmMARval = NULL, df = 0, 
onetailed=FALSE)
}
\arguments{
  \item{outMod1}{
	\code{\linkS4class{SimModelOut}} that saves the analysis result of the first model from the target dataset
}
  \item{outMod2}{
	\code{\linkS4class{SimModelOut}} that saves the analysis result of the second model from the target dataset
}
  \item{dat1Mod1}{
	\code{\linkS4class{SimResult}} that saves the simulation of analyzing Model 1 by datasets created from Model 1
}
  \item{dat1Mod2}{
	\code{\linkS4class{SimResult}} that saves the simulation of analyzing Model 2 by datasets created from Model 1
}
  \item{dat2Mod1}{
	\code{\linkS4class{SimResult}} that saves the simulation of analyzing Model 1 by datasets created from Model 2
}
  \item{dat2Mod2}{
	\code{\linkS4class{SimResult}} that saves the simulation of analyzing Model 2 by datasets created from Model 2
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
  \item{onetailed}{
	If \code{TRUE}, the function will convert the \emph{p} value based on two-tailed test. 
}
}
\value{
	This function provides a vector of \emph{p} values based on the comparison of the difference in fit indices from the real data with the simulation results. The \emph{p} values of fit indices are provided, as well as two additional values: \code{andRule} and \code{orRule}. The \code{andRule} is based on the principle that the model is retained only when all fit indices provide good fit. The proportion is calculated from the number of replications that have all fit indices indicating a better model than the observed data. The proportion from the \code{andRule} is the most stringent rule in retaining a hypothesized model. The \code{orRule} is based on the principle that the model is retained only when at least one fit index provides good fit. The proportion is calculated from the number of replications that have at least one fit index indicating a better model than the observed data. The proportion from the \code{orRule} is the most lenient rule in retaining a hypothesized model.
}
\details{
	In comparing fit indices, the \emph{p} value is the proportion of the number of replications that provide less preference for either model 1 or model 2 than the analysis result from the observed data. In two-tailed test, the function will report the proportion of values under the sampling distribution that are more extreme that one obtained from real data. If the resulting \code{p} value is high (> .05) on one model and low (< .05) in the other model, the model with high \code{p} value is preferred. If the \code{p} values are both high or both low, the decision is undetermined.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		%\item \code{\linkS4class{SimModelOut}} to see how to get the analysis result of observed data
		\item \code{\linkS4class{SimResult}} to run a simulation study
		%\item \code{\link{runFit}} to run a simulation study based on the parameter estimates from the analysis result of observed data
	}
}
\examples{
\dontrun{
#library(lavaan)
#loading <- matrix(0, 11, 3)
#loading[1:3, 1] <- NA
#loading[4:7, 2] <- NA
#loading[8:11, 3] <- NA
#path.A <- matrix(0, 3, 3)
#path.A[2:3, 1] <- NA
#path.A[3, 2] <- NA
#param.A <- simParamSEM(LY=loading, BE=path.A)

#model.A <- simModel(param.A, indLab=c(paste("x", 1:3, sep=""), paste("y", 1:8, sep="")))
#out.A <- run(model.A, PoliticalDemocracy)

#path.B <- matrix(0, 3, 3)
#path.B[1:2, 3] <- NA
#path.B[1, 2] <- NA
#param.B <- simParamSEM(LY=loading, BE=path.B)

#model.B <- simModel(param.B, indLab=c(paste("x", 1:3, sep=""), paste("y", 1:8, sep="")))
#out.B <- run(model.B, PoliticalDemocracy)

#u2 <- simUnif(-0.2, 0.2)
#loading.mis <- matrix(NA, 11, 3)
#loading.mis[is.na(loading)] <- 0
#LY.mis <- simMatrix(loading.mis, "u2")
#misspec <- simMisspecSEM(LY=LY.mis)

#output.A.A <- runFit(model.A, PoliticalDemocracy, 5, misspec=misspec)
#output.A.B <- runFit(model.A, PoliticalDemocracy, 5, misspec=misspec, analyzeModel=model.B)
#output.B.A <- runFit(model.B, PoliticalDemocracy, 5, misspec=misspec, analyzeModel=model.A)
#output.B.B <- runFit(model.B, PoliticalDemocracy, 5, misspec=misspec)

# The output may contain some warnings here. When the number of replications increases (e.g., 1000), the warnings should disappear.
#pValueNonNested(out.A, out.B, output.A.A, output.A.B, output.B.A, output.B.B)
}
}
