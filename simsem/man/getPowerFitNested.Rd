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
getPowerFitNested(altNested, altParent, cutoff, revDirec = FALSE, usedFit = NULL, ...)
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
  \item{revDirec}{
	The default is to count the proportion of fit indices that indicates lower fit to the model, such as 
	how many RMSEA in the alternative model that is worse than cutoffs. The direction can be reversed by setting as \code{TRUE}.
}
  \item{usedFit}{
	Vector of names of fit indices that researchers wish to get power from. The default is to get the powers of all fit indices.  
  }
  \item{\dots}{
	Additional arguments 
  }
}
\section{Methods}{
	\describe{
	\item{signature(altNested="SimResult", altParent="SimResult", cutoff="vector")}{
		This method will find the the differences in fit indices from \code{altNested} and \code{altParent} that provides worse fit than the \code{cutoff}. The additional arguments are \code{nVal}, \code{pmMCARval}, \code{pmMARval}, \code{condCutoff}, and \code{df}, which are needed when using varying sample sizes or percent missing across replications in \code{\linkS4class{SimResult}}. The \code{nVal} is the sample size value that researchers wish to find the fit indices cutoffs from. The \code{pmMCARval} is the percent missing completely at random value that researchers wish to find the fit indices cutoffs from. The \code{pmMARval} is the percent missing at random value that researchers wish to find the fit indices cutoffs from. The \code{condCutoff} is a logical. If \code{TRUE}, the cutoff is applicable only a given set of \code{nVal}, \code{pmMCARval}, and \code{pmMARval}. If \code{FALSE}, the cutoff is applicable in any values of sample size and percent missing. The \code{df} is the degree of freedom used in spline method in predicting the fit indices by the predictors. If \code{df} is 0, the spline method will not be applied.
	}
	\item{signature(altNested="SimResult", altParent="SimResult", cutoff="missing")}{
		The details are similar to the method for \code{altNested="SimResult"}, \code{altParent="SimResult"}, and \code{cutoff="vector"}. The \code{cutoff} argument must not be specified. Rather, the \code{nullNested} and \code{nullParent}, which are additional arguments of this method, are required. The \code{nullNested} is the \code{\linkS4class{SimResult}} that saves the simulation result of the nested model when the nested model is \code{TRUE}. The \code{nullParent} is the \code{\linkS4class{SimResult}} that saves the simulation result of the parent model when the nested model is \code{TRUE}.
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
		\item \code{\link{getCutoff}} to find the cutoffs from null model.
		\item \code{\linkS4class{SimResult}} to see how to create simResult
	}
}
\examples{
u2 <- simUnif(-0.2, 0.2)
n1 <- simNorm(0, 0.1)
u79 <- simUnif(0.7, 0.9)

loading.null <- matrix(0, 6, 1)
loading.null[1:6, 1] <- NA
LX.NULL <- simMatrix(loading.null, 0.7)
RPH.NULL <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model.NULL <- simSetCFA(LY = LX.NULL, RPS = RPH.NULL, RTE = RTD)

error.cor.mis <- matrix(NA, 6, 6)
diag(error.cor.mis) <- 1
RTD.Mis <- symMatrix(error.cor.mis, "rnorm(1,0,0.1)")
CFA.Model.NULL.Mis <- simMisspecCFA(RTE = RTD.Mis)

loading.alt <- matrix(0, 6, 2)
loading.alt[1:3, 1] <- NA
loading.alt[4:6, 2] <- NA
LX.ALT <- simMatrix(loading.alt, 0.7)
latent.cor.alt <- matrix(NA, 2, 2)
diag(latent.cor.alt) <- 1
RPH.ALT <- symMatrix(latent.cor.alt, 0.7)
CFA.Model.ALT <- simSetCFA(LY = LX.ALT, RPS = RPH.ALT, RTE = RTD)

# loading.alt.mis <- matrix(NA, 6, 2)
# loading.alt.mis[is.na(loading.alt)] <- 0
# LX.alt.mis <- simMatrix(loading.alt.mis, "runif(1,-.2,.2)")
# CFA.Model.alt.mis <- simMisspecCFA(LY = LX.alt.mis, RTE=RTD.Mis)

SimData.NULL <- simData(CFA.Model.NULL, 500)
SimData.ALT <- simData(CFA.Model.ALT, 500)

SimModel.NULL <- simModel(CFA.Model.NULL)
SimModel.ALT <- simModel(CFA.Model.ALT)

Output.NULL.NULL <- simResult(10, SimData.NULL, SimModel.NULL)
Output.ALT.NULL <- simResult(10, SimData.ALT, SimModel.NULL)
Output.NULL.ALT <- simResult(10, SimData.NULL, SimModel.ALT)
Output.ALT.ALT <- simResult(10, SimData.ALT, SimModel.ALT)

getPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, nullNested=Output.NULL.NULL, nullParent=Output.NULL.ALT)
getPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, cutoff=c(Chi=3.84, CFI=-0.10))

Output.NULL.NULL2 <- simResult(NULL, SimData.NULL, SimModel.NULL, n=seq(50, 500, 50))
Output.ALT.NULL2 <- simResult(NULL, SimData.ALT, SimModel.NULL, n=seq(50, 500, 50))
Output.NULL.ALT2 <- simResult(NULL, SimData.NULL, SimModel.ALT, n=seq(50, 500, 50))
Output.ALT.ALT2 <- simResult(NULL, SimData.ALT, SimModel.ALT, n=seq(50, 500, 50))

getPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, nullNested=Output.NULL.NULL, nullParent=Output.NULL.ALT, nVal = 250)
getPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, cutoff=c(Chi=3.84, CFI=-0.10), nVal = 250)
}
