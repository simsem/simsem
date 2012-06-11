\name{getPowerFit}
\alias{getPowerFit}
\alias{getPowerFit-methods}
\alias{getPowerFit,data.frame,vector-method}
\alias{getPowerFit,matrix,vector-method}
\alias{getPowerFit,SimResult,vector-method}
\alias{getPowerFit,SimResult,missing-method}
\title{
	Find power in rejecting alternative models based on fit indices criteria
}
\description{
	Find the proportion of fit indices that indicate worse fit than a specified cutoffs. The cutoffs may be calculated from \code{\link{getCutoff}} of the null model.
}
\usage{
getPowerFit(altObject, cutoff, revDirec = FALSE, usedFit=NULL, ...)
}
\arguments{
  \item{altObject}{
	\code{\linkS4class{SimResult}} that indicates alternative model that users wish to reject
}
  \item{cutoff}{
	Fit indices cutoffs from null model or users. This should be a vector with a specified fit indices names as the name of vector elements. This argument can be missing if the \code{\linkS4class{SimResult}} is specified in the \code{altObject} and the \code{\linkS4class{SimResult}} of the null model is specified.
}
  \item{revDirec}{
	The default is to count the proportion of fit indices that indicates lower fit to the model, such as 
	how many RMSEA in the alternative model that is worse than cutoffs. The direction can be reversed by setting as \code{TRUE}.
}
  \item{usedFit}{
	Vector of names of fit indices that researchers wish to getCutoffs from. The default is to getCutoffs of all fit indices.  
  }
  \item{\dots}{
	Additional arguments 
  }
}
\section{Methods}{
	\describe{
	\item{signature(altObject="data.frame", cutoff="vector")}{
		This method will find the fit indices indicated in the \code{altObject} that provides worse fit than the \code{cutoff}. The additional arguments are \code{predictor}, \code{predictorVal}, \code{condCutoff}, and \code{df}, which allows the fit indices predicted by any arbitrary independent variables (such as sample size or percent MCAR). The \code{predictor} is the \code{data.frame} of the predictor values. The number of rows of the \code{predictor} argument should be equal to the number of rows in the \code{object}. The \code{predictorVal} is the values of predictor that researchers would like to find the power from. The \code{condCutoff} is a logical. If \code{TRUE}, the cutoff is applicable only a given value of \code{predictorVal}. If \code{FALSE}, the cutoff is applicable in any values of \code{predictor}. The \code{df} is the degree of freedom used in spline method in predicting the fit indices by the predictors. If \code{df} is 0, the spline method will not be applied. 
	}
	\item{signature(altObject="matrix", cutoff="vector")}{
		The details are similar to the method for \code{altObject="data.frame"} and \code{cutoff="vector"}.
	}
	\item{signature(altObject="SimResult", cutoff="vector")}{
		This method will find the fit indices indicated in the \code{altObject} that provides worse fit than the \code{cutoff}. The additional arguments are \code{nVal}, \code{pmMCARval}, \code{pmMARval}, \code{condCutoff}, and \code{df}, which are needed when using varying sample sizes or percent missing across replications in \code{\linkS4class{SimResult}}. The \code{nVal} is the sample size value that researchers wish to find the fit indices cutoffs from. The \code{pmMCARval} is the percent missing completely at random value that researchers wish to find the fit indices cutoffs from. The \code{pmMARval} is the percent missing at random value that researchers wish to find the fit indices cutoffs from. The \code{condCutoff} is a logical. If \code{TRUE}, the cutoff is applicable only a given set of \code{nVal}, \code{pmMCARval}, and \code{pmMARval}. If \code{FALSE}, the cutoff is applicable in any values of sample size and percent missing. The \code{df} is the degree of freedom used in spline method in predicting the fit indices by the predictors. If \code{df} is 0, the spline method will not be applied.
	}
	\item{signature(altObject="SimResult", cutoff="missing")}{
		The details are similar to the method for \code{altObject="SimResult"} and \code{cutoff="vector"}. The \code{cutoff} argument must not be specified. Rather, the \code{nullObject}, which is an additional argument of this method, is required. The \code{nullObject} is the \code{\linkS4class{SimResult}} that contains the simulation result from fitting the null model by the data from the null model.
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
loading.null <- matrix(0, 6, 1)
loading.null[1:6, 1] <- NA
LX.NULL <- simMatrix(loading.null, 0.7)
RPH.NULL <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model.NULL <- simSetCFA(LY = LX.NULL, RPS = RPH.NULL, RTE = RTD)
SimData.NULL <- simData(CFA.Model.NULL, 500)
SimModel <- simModel(CFA.Model.NULL)
# We make the examples running only 5 replications to save time.
# In reality, more replications are needed.
Output.NULL <- simResult(5, SimData.NULL, SimModel)
Cut.NULL <- getCutoff(Output.NULL, 0.95)

u79 <- simUnif(0.7, 0.9)
loading.alt <- matrix(0, 6, 2)
loading.alt[1:3, 1] <- NA
loading.alt[4:6, 2] <- NA
LX.ALT <- simMatrix(loading.alt, 0.7)
latent.cor.alt <- matrix(NA, 2, 2)
diag(latent.cor.alt) <- 1
RPH.ALT <- symMatrix(latent.cor.alt, "u79")
CFA.Model.ALT <- simSetCFA(LY = LX.ALT, RPS = RPH.ALT, RTE = RTD)
SimData.ALT <- simData(CFA.Model.ALT, 500)
Output.ALT <- simResult(5, SimData.ALT, SimModel)
getPowerFit(Output.ALT, cutoff=Cut.NULL)
Rule.of.thumb <- c(RMSEA=0.05, CFI=0.95, TLI=0.95, SRMR=0.06)
getPowerFit(Output.ALT, cutoff=Rule.of.thumb, usedFit=c("RMSEA", "CFI", "TLI", "SRMR"))

Output.NULL2 <- simResult(NULL, SimData.NULL, SimModel, n=seq(50, 500, 50))
Output.ALT2 <- simResult(NULL, SimData.ALT, SimModel, n=seq(50, 500, 50))
getPowerFit(Output.ALT2, nullObject=Output.NULL2, nVal=250)

}
