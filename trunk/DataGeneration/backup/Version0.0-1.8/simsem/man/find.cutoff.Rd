\name{find.cutoff}
\alias{find.cutoff}
\alias{find.cutoff-methods}
\alias{find.cutoff,data.frame-method}
\alias{find.cutoff,matrix-method}
\alias{find.cutoff,simResult-method}
\title{
	Find cutoff given a priori alpha level
}
\description{
	Extract fit indices information from the \code{\linkS4class{simResult}} and find cutoff of fit indices given a priori alpha level
}
\usage{
find.cutoff(object, alpha, reverse=FALSE, used.fit=NULL)
}
\arguments{
  \item{object}{
	\code{\linkS4class{simResult}} that saves the analysis results from multiple replications
}
  \item{alpha}{
	A priori alpha level
}
  \item{reverse}{
	The default is to find criticl point on the side that indicates worse fit (the right side of RMSEA or the left side of CFI). If specifying as \code{TRUE}, the directions are reversed.
}
  \item{used.fit}{
	Vector of names of fit indices that researchers wish to find cutoffs from. The default is to find cutoffs of all fit indices.
  }
}
\value{
	One-tailed cutoffs of several fit indices with a priori alpha level 
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\code{\linkS4class{simResult}} for a detail of result object
}
\examples{
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- matrix.object(loading, loadingValues)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
PH <- sym.matrix.object(latent.cor, 0.5)
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
TD <- sym.matrix.object(error.cor)
CFA.Model <- matrix.CFA.object(LY = LX, PS = PH, TE = TD)
SimData <- data.object(200, CFA.Model)
SimModel <- model.object(CFA.Model)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
Output <- result.object(SimData, SimModel, 50)
find.cutoff(Output, 0.05)
}
