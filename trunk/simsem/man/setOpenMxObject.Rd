\name{setOpenMxObject}
\alias{setOpenMxObject}
\alias{setOpenMxObject-methods}
\alias{setOpenMxObject,ANY,ANY-method}
\alias{setOpenMxObject,vector,vector-method}
\alias{setOpenMxObject,matrix,matrix-method}
\alias{setOpenMxObject,SimParam,SimRSet-method}
\title{
	Rearrange starting values for \code{OpenMx}
}
\description{
	Rearrange starting values such that it is appropriate for \code{OpenMx} matrix specification such that free parameters are set to be \code{TRUE/FALSE} and values meaning be both fixed value or starting values
}
\usage{
setOpenMxObject(param, start)
}
\arguments{
  \item{param}{
	Set of free parameters (\code{NA} = free parameters; numbers = fixed value with a specified number)
}
  \item{start}{
	Parameter/Starting values of all free parameters
}
}
\section{Methods}{
	\describe{
	\item{signature(param="vector", start="vector")}{
		This function will add fixed value from parameters vector into starting value vector
	}
	\item{signature(param="matrix", start="matrix")}{
		This function will add fixed value from parameters matrix into starting value matrix
	}
	\item{signature(param="SimParam", start="SimRSet")}{
		This function will put all fixed values in \code{\linkS4class{SimParam}} into set of starting value matrices, \code{\linkS4class{SimRSet}}
	}
}}
\value{
	A vector, a matrix, or a \code{\linkS4class{SimRSet}} which includes numbers of fixed parameters and starting values of free parameters.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
# This function is not public

# parameter <- c(NA, NA, 0, 0)
# startingValues <- c(2, 5, 0, 0)
# setOpenMxObject(parameter, startingValues)
}
