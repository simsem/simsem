\name{plotCutoff}
\alias{plotCutoff}
\alias{plotCutoff-methods}
\alias{plotCutoff,data.frame-method}
\alias{plotCutoff,SimResult-method}
\title{
Plot sampling distributions of fit indices
}
\description{
This function will plot sampling distributions of null hypothesis fit indices. The users may add cutoffs by specifying the \code{alpha} level.
}
\usage{
plotCutoff(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	The object (\code{\linkS4class{SimResult}} or \code{data.frame}) that contains values of fit indices in each distribution.
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
	\itemize{
		\item \code{cutoff}: A priori cutoffs for fit indices, saved in a vector
		\item \code{alpha}:	A priori alpha level to getCutoffs of fit indices (do not specify when you have \code{cutoff})
		\item \code{revDirec}:	The default is to find critical point on the side that indicates worse fit (the right side of RMSEA or the left side of CFI). If specifying as TRUE, the directions are reversed.
		\item \code{usedFit}:	The name of fit indices that researchers wish to plot
		\item \code{useContour}:	If there are two of sample size, percent completely at random, and percent missing at random are varying, the \code{plotCutoff} function will provide 3D graph. Contour graph is a default. However, if this is specified as \code{FALSE}, perspective plot is used.
	}
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimResult}} for simResult that used in this function.
		\item \code{\link{getCutoff}} to find values of cutoffs based on null hypothesis sampling distributions only
	}
}
\examples{
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- simMatrix(loading, loadingValues)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, 0.5)
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
RTD <- symMatrix(error.cor)
CFA.Model <- simSetCFA(LY = LX, RPS = RPH, RTE = RTD)
SimData <- simData(200, CFA.Model)
SimModel <- simModel(CFA.Model)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
Output <- simResult(50, SimData, SimModel)
plotCutoff(Output, 0.05, usedFit=c("RMSEA", "SRMR", "CFI", "TLI"))

# Varying N
Output2 <- simResult(NULL, SimData, SimModel, n=51:100)
plotCutoff(Output2, 0.05)

# Varying N and pmMCAR
Output3 <- simResult(NULL, SimData, SimModel, n=51:100, pmMCAR=c(0, 0.05, 0.1, 0.15))
plotCutoff(Output3, 0.05)
}
