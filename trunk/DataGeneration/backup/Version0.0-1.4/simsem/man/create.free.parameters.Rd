\name{create.free.parameters}
\alias{create.free.parameters}
\title{
	Create free parameters object from model specification
}
\description{
	Create free parameters object from model specification
}
\usage{
create.free.parameters(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	\code{simMatrixSet} object
}
}
\value{
	\code{freeParamSet} object
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\note{
	Internal Function
}
\examples{
#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#loadingValues[1:3, 1] <- 0.7
#loadingValues[4:6, 2] <- 0.7
#LX <- matrix.object(loading, loadingValues)

#latent.cor <- matrix(NA, 2, 2)
#diag(latent.cor) <- 1
#PH <- sym.matrix.object(latent.cor, 0.5)

#error.cor <- matrix(0, 6, 6)
#diag(error.cor) <- 1
#TD <- sym.matrix.object(error.cor)

#indicator.mean <- rep(NA, 6)
#MX <- vector.object(indicator.mean, 0)

#CFA.Model <- matrix.CFA.object(LX = LX, PH = PH, TD = TD, MX = MX)
#free <- create.free.parameters(CFA.Model)
}

