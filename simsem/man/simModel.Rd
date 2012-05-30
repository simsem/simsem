\name{simModel}
\alias{simModel}
\alias{simModel-methods}
\alias{simModel,ANY-method}
\alias{simModel,SimSet-method}
\alias{simModel,SimParam-method}
\alias{simModel,SimModelOut-method}
\title{
	Create a model object 
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
	\itemize{
		\item \emph{start:} SimRSet.c that saves all starting values in the model.
		\item \emph{equalCon:} SimEqualCon.c that save constraints specified by users. The default is no constraint.
		\item \emph{package:} Desired analysis package
		\item \emph{estimator:} The default is \code{ML} estimator. Other alternatives are \code{GLS}, \code{WLS}, \code{MLM}, \code{MLF}, and \code{MLR}. Check the sem function help file in the \code{lavaan} package for further details
		\item \emph{auxiliary:} The names or the index of the auxiliary variables in the data
		\item \emph{indLab:} The names of the variable in the model. The exogenous indicators should be listed first (from x1) and then endogenous indicators should be listed next (from y1).
		\item \emph{factorLab:} The names of the factors in the model. The exogenous factors should be listed first (from k1) and then endogenous factors should be listed next (from y1).
	}
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
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
RPH <- symMatrix(latent.cor, 0.5)
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
RTD <- symMatrix(error.cor)
CFA.Model <- simSetCFA(LX = LX, RPH = RPH, RTD = RTD)
SimModel <- simModel(CFA.Model)

library(lavaan)
loading <- matrix(0, 9, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
HS.Model <- simParamCFA(LX = loading)
SimModel <- simModel(HS.Model, indLab=paste("x", 1:9, sep=""))
out <- run(SimModel, HolzingerSwineford1939)
summary(out)
}
