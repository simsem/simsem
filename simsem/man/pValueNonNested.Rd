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
	\code{lavaan-class} that saves the analysis result of the first model from the target dataset
}
  \item{outMod2}{
	\code{lavaan-class} that saves the analysis result of the second model from the target dataset
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
    Sunthud Pornprasertmanit (\email{psunthud@gmail.com})
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
# Model A; Factor 1 --> Factor 2; Factor 2 --> Factor 3
library(lavaan)
loading <- matrix(0, 11, 3)
loading[1:3, 1] <- NA
loading[4:7, 2] <- NA
loading[8:11, 3] <- NA
path.A <- matrix(0, 3, 3)
path.A[2, 1] <- NA
path.A[3, 2] <- NA
model.A <- estmodel(LY=loading, BE=path.A, modelType="SEM", indLab=c(paste("x", 1:3, sep=""), 
	paste("y", 1:8, sep="")))

out.A <- analyze(model.A, PoliticalDemocracy)

# Model A; Factor 1 --> Factor 3; Factor 3 --> Factor 2
path.B <- matrix(0, 3, 3)
path.B[3, 1] <- NA
path.B[2, 3] <- NA
model.B <- estmodel(LY=loading, BE=path.B, modelType="SEM", indLab=c(paste("x", 1:3, sep=""), 
	paste("y", 1:8, sep="")))

out.B <- analyze(model.B, PoliticalDemocracy)

loading.mis <- matrix("runif(1, -0.2, 0.2)", 11, 3)
loading.mis[is.na(loading)] <- 0

# Create SimSem object for data generation and data analysis template
datamodel.A <- model.lavaan(out.A, std=TRUE, LY=loading.mis)
datamodel.B <- model.lavaan(out.B, std=TRUE, LY=loading.mis)

# Get sample size
n <- nrow(PoliticalDemocracy)

# The actual number of replications should be greater than 20.
output.A.A <- sim(20, n=n, model.A, generate=datamodel.A) 
output.A.B <- sim(20, n=n, model.B, generate=datamodel.A)
output.B.A <- sim(20, n=n, model.A, generate=datamodel.B)
output.B.B <- sim(20, n=n, model.B, generate=datamodel.B)

# Find the p-value comparing the observed fit indices against the simulated 
# sampling distribution of fit indices

pValueNonNested(out.A, out.B, output.A.A, output.A.B, output.B.A, output.B.B)

# If the p-value for model A is significant but the p-value for model B is not
# significant, model B is preferred.
}
}
