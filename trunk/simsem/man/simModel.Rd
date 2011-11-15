\name{simModel}
\alias{simModel}
\alias{simModel-methods}
\alias{simModel,ANY-method}
\alias{simModel,SimSet-method}
\alias{simModel,SimFreeParam-method}
\title{
	Create simModel from model specification and be ready for data analysis.
}
\description{
	This function will take model specification from \code{\linkS4class{SimSet}} that contains free parameters, starting values, and fixed values. 
	It will transform the code to a specified SEM package and ready to analyze data.
}
\usage{
simModel(object, ...)
}
\arguments{
  \item{object}{
	\code{\linkS4class{SimSet}} that provides model specification
}
  \item{\dots}{
	Other values that will be explained specifically for each class
}
}
\value{
	\code{\linkS4class{SimModel}} that will be used for data analysis
}
\section{Details in \dots}{
	\enumerate{
		\item \emph{Constraint:} simConstrint.c that save constraints specified by users. The default is no constraint.
		\item \emph{Program:} Desired analysis package
	}
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\enumerate{
		\item \code{\linkS4class{SimModel}} for the simResult
		\item \code{\linkS4class{SimSet}} for the target object containing model specification
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
PH <- symMatrix(latent.cor, 0.5)
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
TD <- symMatrix(error.cor)
CFA.Model <- simSetCFA(LX = LX, PH = PH, TD = TD)
SimModel <- simModel(CFA.Model)
}
