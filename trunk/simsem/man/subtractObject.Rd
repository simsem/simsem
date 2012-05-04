\name{subtractObject}
\alias{subtractObject}
\alias{subtractObject-methods}
\alias{subtractObject,ANY,ANY-method}
\alias{subtractObject,SimRSet,SimRSet-method}
\title{
	Make a subtraction of each element in an object
}
\description{
	Make a subtraction of each element in an object. For example, subtract the parameter estimates by the paramter values
}
\usage{
subtractObject(object1, object2)
}
\arguments{
  \item{object1}{
	The first object
}
  \item{object2}{
	The second object
}
}
\section{Methods}{
	\describe{
	\item{signature(object1="SimRSet", object2="SimRSet")}{
		This function will find the bias by subtracting for parameter estimates from the real parameters.
	}
}}
\value{
	The object after subtraction
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
# This function is not public

#u89 <- simUnif(0.8, 0.9)
#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#LX <- simMatrix(loading, "u89")
#startingValues(LX, 10)

#u89 <- simUnif(0.8, 0.9)
#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#LX <- simMatrix(loading, "u89")
#latent.cor <- matrix(NA, 2, 2)
#diag(latent.cor) <- 1
#PH <- symMatrix(latent.cor, 0.5)
#error.cor <- matrix(0, 6, 6)
#diag(error.cor) <- 1
#TD <- symMatrix(error.cor)
#CFA.Model <- simSetCFA(LX = LX, PH = PH, TD = TD)
#result <- startingValues(CFA.Model, 10)
#summary(result)
}
