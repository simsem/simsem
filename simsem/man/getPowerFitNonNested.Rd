\name{getPowerFitNonNested}
\alias{getPowerFitNonNested}
\alias{getPowerFitNonNested-methods}
\alias{getPowerFitNonNested,SimResult,SimResult,vector-method}
\alias{getPowerFitNonNested,SimResult,SimResult,missing-method}
\title{
	Find power in rejecting non-nested models based on the differences in fit indices 
}
\description{
	Find the proportion of the difference in fit indices from one model that does not in the range of sampling distribution from another model (reject that the dataset comes from the second model) or indicates worse fit than a specified cutoff. 
}
\usage{
getPowerFitNonNested(dat2Mod1, dat2Mod2, cutoff = NULL, dat1Mod1 = NULL, 
dat1Mod2 = NULL, revDirec = FALSE, usedFit = NULL, alpha = 0.05, nVal = NULL, 
pmMCARval = NULL, pmMARval = NULL, condCutoff = TRUE, df = 0, onetailed = FALSE)
}
\arguments{
  \item{dat2Mod1}{
	\code{\linkS4class{SimResult}} that saves the simulation of analyzing Model 1 by datasets created from Model 2
}
  \item{dat2Mod2}{
	\code{\linkS4class{SimResult}} that saves the simulation of analyzing Model 2 by datasets created from Model 2
}
  \item{cutoff}{
	A vector of priori cutoffs for fit indices. The \code{cutoff} cannot be specified if the \code{dat1Mod1} and \code{dat1Mod2} are specified.
}
  \item{dat1Mod1}{
	The \code{\linkS4class{SimResult}} that saves the simulation of analyzing Model 1 by datasets created from Model 1. This argument must be specified with \code{dat1Mod2}. The \code{dat1Mod1} cannot be specified if the \code{cutoff} is specified.
}
  \item{dat1Mod2}{
	The \code{\linkS4class{SimResult}} that saves the simulation of analyzing Model 2 by datasets created from Model 1. This argument must be specified with \code{dat1Mod1}. The \code{dat1Mod2} cannot be specified if the \code{cutoff} is specified.
}
  \item{revDirec}{
	Reverse the direction of deciding a power by fit indices (e.g., less than --> greater than). The default is to count the proportion of fit indices that indicates lower fit to the model, such as how many RMSEA in the alternative model that is worse than cutoffs. The direction can be reversed by setting as \code{TRUE}.
}
  \item{usedFit}{
	The vector of names of fit indices that researchers wish to get powers from. The default is to get powers of all fit indices
}
  \item{alpha}{
	The alpha level used to find the cutoff if the \code{nullObject} is specified. This argument is not applicable if the \code{cutoff} is specified.
}
  \item{nVal}{
	The sample size value that researchers wish to find the power from. This argument is applicable when \code{altObject} has a random sample size. 
}
  \item{pmMCARval}{
	The percent missing completely at random value that researchers wish to find the power from. This argument is applicable when \code{altObject} has a random percent missing completely at random. 
}
  \item{pmMARval}{
	The percent missing at random value that researchers wish to find the power from. This argument is applicable when \code{altObject} has a random percent missing at random. 
}
  \item{condCutoff}{
	A logical value to use a conditional quantile method (if \code{TRUE}) or logistic regression method (if \code{FALSE}) to find the power. The conditional quantile method use quantile regression to find the quantile of the cutoff on the alternative sampling distribution that varies \code{nVal}, \code{pmMCARval}, or \code{pmMARval}. The value of 1 - quantile will be reported as the power given the set of \code{nVal}, \code{pmMCARval}, and \code{pmMARval}. The logistic regression method is based on transforming the fit indices value to reject/retain decision first. Then, the logistic regression is used to predict reject/retain decision given the set of \code{nVal}, \code{pmMCARval}, and \code{pmMARval}. The predicted probability is reported as a power. This argument is applicable for specification of \code{cutoff} only.
}
  \item{df}{
	The degree of freedom used in spline method in quantile regression (\code{condCutoff = TRUE}). If \code{df} is 0, which is the default, the spline method will not be applied.
}
  \item{onetailed}{
	Derive the cutoff by using one-tailed test if specified as \code{TRUE}. This argument is applicable only when \code{dat1Mod1} and \code{dat1Mod2} are specified.
}
}
\value{
	List of power given different fit indices.
}
\author{
    Sunthud Pornprasertmanit (\email{psunthud@gmail.com})
}
\seealso{
	\itemize{
		\item \code{\link{getCutoffNonNested}} to find the cutoffs for non-nested model comparison
		\item \code{\linkS4class{SimResult}} to see how to create simResult
	}
}
\examples{
\dontrun{
# Model A: Factor 1 on Items 1-3 and Factor 2 on Items 4-8
loading.A <- matrix(0, 8, 2)
loading.A[1:3, 1] <- NA
loading.A[4:8, 2] <- NA
LY.A <- bind(loading.A, 0.7)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, "runif(1, 0.7, 0.9)")
RTE <- binds(diag(8))
CFA.Model.A <- model(LY = LY.A, RPS = RPS, RTE = RTE, modelType="CFA")

# Model B: Factor 1 on Items 1-4 and Factor 2 on Items 5-8
loading.B <- matrix(0, 8, 2)
loading.B[1:4, 1] <- NA
loading.B[5:8, 2] <- NA
LY.B <- bind(loading.B, 0.7)
CFA.Model.B <- model(LY = LY.B, RPS = RPS, RTE = RTE, modelType="CFA")

# The actual number of replications should be greater than 10.
Output.A.A <- sim(10, n=500, model=CFA.Model.A, generate=CFA.Model.A) 
Output.A.B <- sim(10, n=500, model=CFA.Model.B, generate=CFA.Model.A) 
Output.B.A <- sim(10, n=500, model=CFA.Model.A, generate=CFA.Model.B) 
Output.B.B <- sim(10, n=500, model=CFA.Model.B, generate=CFA.Model.B) 

# Find the power based on the derived cutoff for both models
getPowerFitNonNested(Output.B.A, Output.B.B, dat1Mod1=Output.A.A, dat1Mod2=Output.A.B)

# Find the power based on the AIC and BIC of 0 (select model B if Output.B.B has lower AIC or BIC)
getPowerFitNonNested(Output.B.A, Output.B.B, cutoff=c(AIC=0, BIC=0))
}
}
