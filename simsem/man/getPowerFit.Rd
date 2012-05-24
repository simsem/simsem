\name{getPowerFit}
\alias{getPowerFit}
\alias{getPowerFit-methods}
\alias{getPowerFit,data.frame-method}
\alias{getPowerFit,matrix-method}
\alias{getPowerFit,SimResult-method}
\title{
	Find power in rejecting alternative models based on fit indices criteria
}
\description{
	Find the proportion of fit indices that indicate worse fit than a specified cutoffs. The cutoffs may be calculated from \code{\link{getCutoff}} of the null model.
}
\usage{
getPowerFit(altObject, cutoff, revDirec = FALSE, usedFit=NULL)
}
\arguments{
  \item{altObject}{
	\code{\linkS4class{SimResult}} that indicates alternative model that users wish to reject
}
  \item{cutoff}{
	Fit indices cutoffs from null model or users. This should be a vector with a specified fit indices names as the name of vector elements. 
	The best way to specify cutoff is to calculate from \code{\link{getCutoff}} function.
}
  \item{revDirec}{
	The default is to count the proportion of fit indices that indicates lower fit to the model, such as 
	how many RMSEA in the alternative model that is worse than cutoffs. The direction can be reversed by setting as \code{TRUE}.
}
  \item{usedFit}{
	Vector of names of fit indices that researchers wish to getCutoffs from. The default is to getCutoffs of all fit indices.  
  }
}
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
getPowerFit(Output.ALT, Cut.NULL)
Rule.of.thumb <- c(RMSEA=0.05, CFI=0.95, TLI=0.95, SRMR=0.06)
getPowerFit(Output.ALT, Rule.of.thumb, usedFit=c("RMSEA", "CFI", "TLI", "SRMR"))
}
