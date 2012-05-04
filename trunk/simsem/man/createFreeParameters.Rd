\name{createFreeParameters}
\alias{createFreeParameters}
\title{
	Create a free parameters object from a model specification
}
\description{
	Create a free parameters object, \code{\linkS4class{SimParam}}, from a model specification, \code{\linkS4class{SimSet}}
}
\usage{
createFreeParameters(object)
}
\arguments{
  \item{object}{
	The model specification in \code{\linkS4class{SimSet}} that saves the free parameter information
}
}
\value{
	The free parameter object, \code{\linkS4class{SimParam}}
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
# No comment out because this function is not public

# loading <- matrix(0, 6, 2)
# loading[1:3, 1] <- NA
# loading[4:6, 2] <- NA
# loadingValues <- matrix(0, 6, 2)
# loadingValues[1:3, 1] <- 0.7
# loadingValues[4:6, 2] <- 0.7
# LX <- simMatrix(loading, loadingValues)
# latent.cor <- matrix(NA, 2, 2)
# diag(latent.cor) <- 1
# RPH <- symMatrix(latent.cor, 0.5)
# error.cor <- matrix(0, 6, 6)
# diag(error.cor) <- 1
# RTD <- symMatrix(error.cor)
# indicator.mean <- rep(NA, 6)
# MX <- simVector(indicator.mean, 0)
# CFA.Model <- simSetCFA(LX = LX, RPH = RPH, RTD = RTD, MX = MX)
# free <- createFreeParameters(CFA.Model)
}
