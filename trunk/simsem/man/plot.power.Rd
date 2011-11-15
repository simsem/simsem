\name{plotPower}
\alias{plotPower}
\alias{plotPower-methods}
\alias{plotPower,data.frame,data.frame-method}
\alias{plotPower,data.frame,vector-method}
\alias{plotPower,SimResult,SimResult-method}
\alias{plotPower,SimResult,vector-method}
\title{
Plot sampling distributions of fit indices that visualize power
}
\description{
This function will plot sampling distributions of fit indices that visualize power in either a histogram or overlapping histograms.
}
\usage{
plotPower(object.alt, object.null, ...)
}
\arguments{
  \item{object.alt}{
	The object (\code{\linkS4class{SimResult}} or \code{data.frame}) that saves fit indices for alternative hypothesis
}
  \item{object.null}{
	The object that represents null hypothesis. It can be \code{vector} of cutoffs (that might be calculated from \code{\link{getCutoff}} 
	or an object that save raw data of fit indices for null hypothesis (\code{\linkS4class{SimResult}} or \code{data.frame}).
}
  \item{\dots}{
	Other arguments specific to different types of object you pass in the function.
}
}
\value{
NONE. Only plot the fit indices distributions.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\section{Details in \dots}{
	\enumerate{
		\item \code{alpha}:	A priori alpha level to getCutoffs of fit indices (do not specify when you have \code{cutoff})
		\item \code{used.fit}:	The name of fit indices that researchers wish to plot
	}
}
\seealso{
	\enumerate{
		\item \code{\linkS4class{SimResult}} for simResult that used in this function.
		\item \code{\link{getCutoff}} to find values of cutoffs based on null hypothesis sampling distributions only
	}
}
\examples{
loading.null <- matrix(0, 6, 1)
loading.null[1:6, 1] <- NA
LX.NULL <- simMatrix(loading.null, 0.7)
PH.NULL <- symMatrix(diag(1))
TD <- symMatrix(diag(6))
CFA.Model.NULL <- simSetCFA(LY = LX.NULL, PS = PH.NULL, TE = TD)
SimData.NULL <- simData(500, CFA.Model.NULL)
SimModel <- simModel(CFA.Model.NULL)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
Output.NULL <- simResult(SimData.NULL, SimModel, 50)
Cut.NULL <- getCutoff(Output.NULL, 0.95)

u79 <- simUnif(0.7, 0.9)
loading.alt <- matrix(0, 6, 2)
loading.alt[1:3, 1] <- NA
loading.alt[4:6, 2] <- NA
LX.ALT <- simMatrix(loading.alt, 0.7)
latent.cor.alt <- matrix(NA, 2, 2)
diag(latent.cor.alt) <- 1
PH.ALT <- symMatrix(latent.cor.alt, "u79")
CFA.Model.ALT <- simSetCFA(LY = LX.ALT, PS = PH.ALT, TE = TD)
SimData.ALT <- simData(500, CFA.Model.ALT)
Output.ALT <- simResult(SimData.ALT, SimModel, 50)
getPower(Output.ALT, Cut.NULL)
Rule.of.thumb <- c(RMSEA=0.05, CFI=0.95, TLI=0.95, SRMR=0.06)
plotPower(Output.ALT, Output.NULL, alpha=0.05, used.fit=c("RMSEA", "CFI", "TLI", "SRMR"))
}
