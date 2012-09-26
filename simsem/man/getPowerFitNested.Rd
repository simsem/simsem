\name{getPowerFitNested}
\alias{getPowerFitNested}
\alias{getPowerFitNested-methods}
\alias{getPowerFitNested,SimResult,SimResult,vector-method}
\alias{getPowerFitNested,SimResult,SimResult,missing-method}
\title{
	Find power in rejecting nested models based on the differences in fit indices 
}
\description{
	Find the proportion of the difference in fit indices that indicate worse fit than a specified (or internally derived) cutoffs. 
}
\usage{
getPowerFitNested(altNested, altParent, cutoff, ...)
}
\arguments{
  \item{altNested}{
	\code{\linkS4class{SimResult}} that saves the simulation result of the nested model when the nested model is \code{FALSE}.
}
  \item{altParent}{
	\code{\linkS4class{SimResult}} that saves the simulation result of the parent model when the nested model is \code{FALSE}.
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
	\item{signature(altNested="SimResult", altParent="SimResult", cutoff="vector")}{
		This method will find the the differences in fit indices from \code{altNested} and \code{altParent} that provides worse fit than the \code{cutoff}. The additional arguments are \code{revDirec}, \code{usedFit}, \code{nVal}, \code{pmMCARval}, \code{pmMARval}, \code{condCutoff}, and \code{df}, which are needed when using varying sample sizes or percent missing across replications in \code{\linkS4class{SimResult}}. The \code{revDirec} is whether to reverse a direction. The default is to count the proportion of fit indices that indicates lower fit to the model, such as how many RMSEA in the alternative model that is worse than cutoffs. The direction can be reversed by setting as \code{TRUE}. The \code{usedFit} is the vector of names of fit indices that researchers wish to get power from. The default is to get the powers of all fit indices. The \code{nVal} is the sample size value that researchers wish to find the fit indices cutoffs from. The \code{pmMCARval} is the percent missing completely at random value that researchers wish to find the fit indices cutoffs from. The \code{pmMARval} is the percent missing at random value that researchers wish to find the fit indices cutoffs from. The \code{condCutoff} is a logical. If \code{TRUE}, the cutoff is applicable only a given set of \code{nVal}, \code{pmMCARval}, and \code{pmMARval}. If \code{FALSE}, the cutoff is applicable in any values of sample size and percent missing. The \code{df} is the degree of freedom used in spline method in predicting the fit indices by the predictors. If \code{df} is 0, the spline method will not be applied.
	}
	\item{signature(altNested="SimResult", altParent="SimResult", cutoff="missing")}{
		The details are similar to the method for \code{altNested="SimResult"}, \code{altParent="SimResult"}, and \code{cutoff="vector"}. The \code{cutoff} argument must not be specified. Rather, the \code{nullNested} and \code{nullParent}, which are additional arguments of this method, are required. The \code{nullNested} is the \code{\linkS4class{SimResult}} that saves the simulation result of the nested model when the nested model is \code{TRUE}. The \code{nullParent} is the \code{\linkS4class{SimResult}} that saves the simulation result of the parent model when the nested model is \code{TRUE}.
	}
}}
\value{
	List of power given different fit indices. The \code{TraditionalChi} means the proportion of replications that are rejected by the traditional chi-square difference test.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\link{getCutoff}} to find the cutoffs from null model.
		\item \code{\linkS4class{SimResult}} to see how to create simResult
	}
}
\examples{
\dontrun{
# Null model (Nested model) with one factor
loading.null <- matrix(0, 6, 1)
loading.null[1:6, 1] <- NA
LY.NULL <- bind(loading.null, 0.7)
RPS.NULL <- binds(diag(1))
RTE <- binds(diag(6))
CFA.Model.NULL <- model(LY = LY.NULL, RPS = RPS.NULL, RTE = RTE, modelType="CFA")

# Alternative model (Parent model) with two factors
loading.alt <- matrix(0, 6, 2)
loading.alt[1:3, 1] <- NA
loading.alt[4:6, 2] <- NA
LY.ALT <- bind(loading.alt, 0.7)
latent.cor.alt <- matrix(NA, 2, 2)
diag(latent.cor.alt) <- 1
RPS.ALT <- binds(latent.cor.alt, 0.7)
CFA.Model.ALT <- model(LY = LY.ALT, RPS = RPS.ALT, RTE = RTE, modelType="CFA")

# We make the examples running only 10 replications to save time.
# In reality, more replications are needed.
Output.NULL.NULL <- sim(10, n=500, model=CFA.Model.NULL, generate=CFA.Model.NULL) 
Output.ALT.NULL <- sim(10, n=500, model=CFA.Model.NULL, generate=CFA.Model.ALT) 
Output.NULL.ALT <- sim(10, n=500, model=CFA.Model.ALT, generate=CFA.Model.NULL) 
Output.ALT.ALT <- sim(10, n=500, model=CFA.Model.ALT, generate=CFA.Model.ALT) 

# Find the power based on the derived cutoff from the models analyzed on the null datasets
getPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, nullNested=Output.NULL.NULL, nullParent=Output.NULL.ALT)

# Find the power based on the chi-square value at df=1 and the CFI change (intentionally
# use a cutoff from Cheung and Rensvold (2002) in an appropriate situation).
getPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, cutoff=c(Chi=3.84, CFI=-0.10))

# The example of continous varying sample size. Note that more fine-grained 
# values of n is needed, e.g., n=seq(50, 500, 1)
Output.NULL.NULL2 <- sim(NULL, n=seq(50, 500, 50), model=CFA.Model.NULL, generate=CFA.Model.NULL) 
Output.ALT.NULL2 <- sim(NULL, n=seq(50, 500, 50), model=CFA.Model.NULL, generate=CFA.Model.ALT) 
Output.NULL.ALT2 <- sim(NULL, n=seq(50, 500, 50), model=CFA.Model.ALT, generate=CFA.Model.NULL) 
Output.ALT.ALT2 <- sim(NULL, n=seq(50, 500, 50), model=CFA.Model.ALT, generate=CFA.Model.ALT) 

# Get the power based on the derived cutoff from the null model at the sample size of 250
getPowerFitNested(Output.ALT.NULL2, Output.ALT.ALT2, nullNested=Output.NULL.NULL2, nullParent=Output.NULL.ALT2, nVal = 250)

# Get the power based on the rule of thumb from the null model at the sample size of 250
getPowerFitNested(Output.ALT.NULL2, Output.ALT.ALT2, cutoff=c(Chi=3.84, CFI=-0.10), nVal = 250)
}
}
