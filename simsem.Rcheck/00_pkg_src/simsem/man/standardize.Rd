\name{standardize}
\alias{standardize}
\alias{standardize-methods}
\alias{standardize,ANY-method}
\alias{standardize,SimModelOut-method}
\alias{standardize,SimRSet-method}
\title{
	Standardize the parameter estimates within an object
}
\description{
	Standardize the parameter estimates within an object
}
\usage{
standardize(object)
}
\arguments{
  \item{object}{
	The object to be standardized
}
}
\section{Methods}{
	\describe{
	\item{signature(object="SimModelOut")}{
		This function will extract the coefficients and standardize it
	}
	\item{signature(object="SimRSet")}{
		This function will extract the coefficients and standardize it
	}
}}
\value{
	The object in the same class with standarized values
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
# This function is not public.

# loading <- matrix(0, 6, 2)
# loading[1:3, 1] <- NA
# loading[4:6, 2] <- NA
# loadingValues <- matrix(0, 6, 2)
# loadingValues[1:3, 1] <- 0.7
# loadingValues[4:6, 2] <- 0.7
# LX <- simMatrix(loading, loadingValues)
# summary(LX)
# latent.cor <- matrix(NA, 2, 2)
# diag(latent.cor) <- 1
# PH <- symMatrix(latent.cor, 0.5)
# error.cor <- matrix(0, 6, 6)
# diag(error.cor) <- 1
# TD <- symMatrix(error.cor)
# CFA.Model <- simSetCFA(LX = LX, PH = PH, TD = TD)
# SimData <- simData(CFA.Model, 200)
# SimModel <- simModel(CFA.Model)
# standardize(run(SimModel, run(SimData)))

# loading <- matrix(0, 6, 2)
# loading[1:3, 1] <- NA
# loading[4:6, 2] <- NA
# loadingValues <- matrix(0, 6, 2)
# loadingValues[1:3, 1] <- 0.7
# loadingValues[4:6, 2] <- 0.7
# LX <- simMatrix(loading, loadingValues)
# summary(LX)
# latent.cor <- matrix(NA, 2, 2)
# diag(latent.cor) <- 1
# PH <- symMatrix(latent.cor, 0.5)
# error.cor <- matrix(0, 6, 6)
# diag(error.cor) <- 1
# TD <- symMatrix(error.cor)
# CFA.Model <- simSetCFA(LX = LX, PH = PH, TD = TD)
# set <- reduceMatrices(run(CFA.Model))
}
