\name{createImpliedMACS}
\alias{createImpliedMACS}
\alias{createImpliedMACS-methods}
\alias{createImpliedMACS,MatrixSet-method}
\alias{createImpliedMACS,SimRSet-method}
\title{
	Create model implied mean vector and covariance matrix
}
\description{
	Create model implied mean vector and covariance matrix from a set of parameter values
}
\usage{
createImpliedMACS(object, ...)
}
\arguments{
  \item{object}{
	A matrix set containing values of parameters
}
  \item{...}{
	Other objects, such as adding the parameter of model misspecification in the data output object
 }
}
\value{
	A list with (a) \code{M} for a model-implied mean vector and (b) \code{CM} for a model-implied covariance matrix
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
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
param <- run(CFA.Model)
createImpliedMACS(param)
}
