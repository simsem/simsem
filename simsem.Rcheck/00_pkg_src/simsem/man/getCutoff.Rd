\name{getCutoff}
\alias{getCutoff}
\alias{getCutoff-methods}
\alias{getCutoff,data.frame-method}
\alias{getCutoff,matrix-method}
\alias{getCutoff,SimResult-method}
\title{
	Find cutoff given a priori alpha level
}
\description{
	Extract fit indices information from the \code{\linkS4class{SimResult}} and getCutoff of fit indices given a priori alpha level
}
\usage{
getCutoff(object, alpha, revDirec = FALSE, usedFit = NULL)
}
\arguments{
  \item{object}{
	\code{\linkS4class{SimResult}} that saves the analysis results from multiple replications
}
  \item{alpha}{
	A priori alpha level
}
  \item{revDirec}{
	The default is to find criticl point on the side that indicates worse fit (the right side of RMSEA or the left side of CFI). If specifying as \code{TRUE}, the directions are reversed.
}
  \item{usedFit}{
	Vector of names of fit indices that researchers wish to getCutoffs from. The default is to getCutoffs of all fit indices.
  }
}
\value{
	One-tailed cutoffs of several fit indices with a priori alpha level 
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\code{\linkS4class{SimResult}} for a detail of simResult
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
SimData <- simData(CFA.Model, 200)
SimModel <- simModel(CFA.Model)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
Output <- simResult(50, SimData, SimModel)
getCutoff(Output, 0.05)
}
