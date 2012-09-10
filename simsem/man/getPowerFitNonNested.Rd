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
getPowerFitNonNested(dat2Mod1, dat2Mod2, cutoff, ...)
}
\arguments{
  \item{dat2Mod1}{
	\code{\linkS4class{SimResult}} that saves the simulation of analyzing Model 1 by datasets created from Model 2
}
  \item{dat2Mod2}{
	\code{\linkS4class{SimResult}} that saves the simulation of analyzing Model 2 by datasets created from Model 2
}
  \item{cutoff}{
	A vector of priori cutoffs for fit indices.
}
  \item{\dots}{
	Additional arguments 
  }
}
\section{Methods}{
	\describe{
	\item{signature(dat2Mod1="SimResult", dat2Mod2="SimResult", cutoff="vector")}{
		This method will find the the differences in fit indices from \code{dat2Mod1} and \code{dat2Mod2} that provides worse fit than the \code{cutoff}. The additional arguments are \code{revDirec}, \code{usedFit}, \code{nVal}, \code{pmMCARval}, \code{pmMARval}, \code{condCutoff}, and \code{df}, which are needed when using varying sample sizes or percent missing across replications in \code{\linkS4class{SimResult}}. The \code{revDirec} is whether to reverse a direction. The default is to count the proportion of the difference of fit indices that lower than the specified cutoffs, such as how many the difference in RMSEA in the alternative model that is lower than cutoffs. The direction can be reversed by setting as \code{TRUE}. The \code{usedFit} is the vector of names of fit indices that researchers wish to get power from. The default is to get the powers of all fit indices. The \code{nVal} is the sample size value that researchers wish to find the fit indices cutoffs from. The \code{pmMCARval} is the percent missing completely at random value that researchers wish to find the fit indices cutoffs from. The \code{pmMARval} is the percent missing at random value that researchers wish to find the fit indices cutoffs from. The \code{condCutoff} is a logical. If \code{TRUE}, the cutoff is applicable only a given set of \code{nVal}, \code{pmMCARval}, and \code{pmMARval}. If \code{FALSE}, the cutoff is applicable in any values of sample size and percent missing. The \code{df} is the degree of freedom used in spline method in predicting the fit indices by the predictors. If \code{df} is 0, the spline method will not be applied.
	}
	\item{signature(dat2Mod1="SimResult", dat2Mod2="SimResult", cutoff="missing")}{
		The details are similar to the method for \code{dat2Mod1="SimResult"}, \code{dat2Mod2="SimResult"}, and \code{cutoff="vector"}. The \code{cutoff} argument must not be specified. Rather, the \code{dat1Mod1} and \code{dat1Mod2}, which are additional arguments of this method, are required. The \code{dat1Mod1} is the \code{\linkS4class{SimResult}} that saves the simulation of analyzing Model 1 by datasets created from Model 1. The \code{dat1Mod2} is the \code{\linkS4class{SimResult}} that saves the simulation of analyzing Model 2 by datasets created from Model 1. The another additional argument is \code{onetailed} that is to derive the cutoff by using one-tailed test if specified as \code{TRUE}.
	}
}}
\value{
	List of power given different fit indices.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
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
