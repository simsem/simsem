\name{startingValues}
\alias{startingValues}
\alias{startingValues-methods}
\alias{startingValues,ANY-method}
\alias{startingValues,SimMatrix-method}
\alias{startingValues,SimVector-method}
\alias{startingValues,SimSet-method}
\title{
	Find starting values by averaging random numbers
}
\description{
	Find starting values of free parameters based on pre-specified starting values. If the pre-specified starting values are numbers, the function will use that values. If they are distribution object, this function will randomly draw from the distribution 10 times and take the average of those values.
}
\usage{
startingValues(object, trial, ...)
}
\arguments{
  \item{object}{
	The target object that is used to find starting values
}
  \item{trial}{
	The number of random drawn to average out and provide the starting values
}
  \item{\dots}{
	Other arguments, such as reduced for reducing X-Y set of matrices to Y set of matrices only
}
}
\section{Methods}{
	\describe{
	\item{signature(object="SimMatrix")}{
		Draw samples from the \code{\linkS4class{SimMatrix}}, take the average, and report the starting values as a \code{matrix}.
	}
	\item{signature(object="SimVector")}{
		Draw samples from the \code{\linkS4class{SimVector}}, take the average, and report the starting values as a \code{vector}.
	}
	\item{signature(object="SimSet")}{
		Draw samples from \code{\linkS4class{SimSet}}, take the average, and report the starting values as a \code{\linkS4class{MatrixSet}}.
	}
}}
\value{
	A vector, a matrix, or a \code{\linkS4class{MatrixSet}} which includes the starting values of parameters
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
