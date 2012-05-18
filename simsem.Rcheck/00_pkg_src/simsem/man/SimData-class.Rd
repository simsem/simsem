\name{SimData-class}
\Rdversion{1.1}
\docType{class}
\alias{SimData-class}
\alias{run,SimData-method}
\alias{summary,SimData-method}
\title{Class \code{"SimData"}}
\description{
This class will save information for data simulation and can create data by run function.
}
\section{Objects from the Class}{
Objects can be created by \code{\link{simData}}. Also, it can be called by \code{new("SimData", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{modelType}:}{Model type (CFA, Path, or SEM)}
    \item{\code{n}:}{Sample size}
    \item{\code{param}:}{Model specification that used in data generation. It must be in \code{\linkS4class{SimSet}} class.}
    \item{\code{misspec}:}{Model misspecification that used in data generation. It must be in \code{\linkS4class{SimMisspec}} class.}
    \item{\code{equalCon}:}{Equality constraints in data generation. It must be in \code{\linkS4class{SimEqualCon}} class.}
    \item{\code{maxDraw}:}{The maximum number of random drawn parameters and misspecification model until all parameters in the model are eligible (no negative error variance, standardized coefficients over 1).}
    \item{\code{sequential}:}{If \code{TRUE}, use a sequential method to create data such that the data from factor are generated first and apply to a set of equations to obtain the data of indicators. If \code{FALSE}, create data directly from model-implied mean and covariance of indicators.}
    \item{\code{facDist}:}{A \code{\linkS4class{SimDataDist}} for a distribution of factors. Use when \code{sequential} is \code{TRUE}.}
    \item{\code{errorDist}:}{A \code{\linkS4class{SimDataDist}} for a distribution of measurement errors. Use when \code{sequential} is \code{TRUE}.}
    \item{\code{indDist}:}{A \code{\linkS4class{SimDataDist}} for a distribution of indicators. Use when \code{sequential} is \code{FALSE}.}
	\item{\code{indLab}:}{A vector of indicator names. If not specified, the variables names are y1, y2, ... .}
	\item{\code{modelBoot}:}{If \code{TRUE}, use a model-based bootstrap for data generation. See details for further information. This argument need a dataset in the \code{realData} argument.}
	\item{\code{realData}:}{The real dataset that the model based bootstrap will follow the distribution.}
  }
}
\section{Methods}{
  \describe{
    \item{run}{To create data from this class. \code{N} is the additional argument that users may change the sample size when creating data. 
		\code{dataOnly} is default to be \code{TRUE}. If FALSE, the resulting object in \code{\linkS4class{SimDataOut}} can be used to provide details of things used in create the data.}
    \item{summary}{Summarize all attributes in the simData.}
	 }
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{linkS4class{SimSet}} for how to specify data generation model.
		\item \code{linkS4class{SimMisspec}} for how to specify misspecification in this data generation model. 
		\item \code{linkS4class{SimEqualCon}} for how to set equality constraints for data generation.
		\item \code{link{simResult}} for the use of this class to run Monte Carlo simulation.
		\item \code{linkS4class{SimModelOut}} for the output type after the \code{run} function with \code{dataOnly=TRUE}.
	}
}
\examples{
showClass("SimData")
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
SimData <- simData(CFA.Model, 200)
summary(SimData)
run(SimData)
}
\keyword{classes}
