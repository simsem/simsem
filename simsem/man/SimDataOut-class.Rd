\name{SimDataOut-class}
\docType{class}
\alias{SimDataOut-class}
\alias{summary,SimDataOut-method}
\alias{createImpliedMACS,SimDataOut-method}
\alias{summaryPopulation,SimDataOut-method}
\alias{getPopulation,SimDataOut-method}
\title{Class \code{"SimDataOut"}}
\description{
This class will provide the simulated dataset and population behind the generated dataset.
}
\section{Objects from the Class}{
Objects can be created by \code{\link{run}} on the \code{\linkS4class{SimData}} with \code{dataOnly=FALSE}. It can also be called from the form \code{new("SimDataOut", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{modelType}:}{Analysis model type (CFA, Path, or SEM)}
    \item{\code{data}:}{The simulated data}
    \item{\code{param}:}{Model specification that used in data generation. It must be in \code{\linkS4class{SimSet}} class.}
    \item{\code{paramOut}:}{Parameter values underlying the simulated data.}
    \item{\code{misspecOut}:}{Model misspecification underlying the simulated data}
    \item{\code{equalCon}:}{Equality constraints in data generation. It must be in \code{\linkS4class{SimEqualCon}} class.}
	\item{\code{n}:}{Sample size of the created data.}
  }
}
\section{Methods}{
	\itemize{
		\item{\link{summary}} to summarize the object
		\item{\link{createImpliedMACS}} to create the model implied means and covariance matrix from the parameter estimates. The \code{misspec} argument can be specified as \code{TRUE} to create the model implied means and covariance matrix from both parameters and model misspecification
		\item \code{\link{summaryPopulation}} to summarize the data generation population underlying the data.
		\item \code{\link{getPopulation}} to extract the data generation population. The additional argument is \code{misspec}. If \code{TRUE}, the data generation population with model misspecification will be returned. If \code{FALSE}, the data generation population without model misspecification will be returned.
	}
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimData}} The object used as data template for simulated data.
		\item \code{\linkS4class{SimModel}} The object used as analysis		
	}
}
\examples{
showClass("SimDataOut")
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- simMatrix(loading, 0.7)
RPH <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model <- simSetCFA(LY = LX, RPS = RPH, RTE = RTD)
SimData <- simData(CFA.Model, 500)
SimModel <- simModel(CFA.Model)
Data <- run(SimData, dataOnly=FALSE)
Result <- run(SimModel, Data)
summary(Data)
summaryPopulation(Data)
mis <- getPopulation(Data, misspec=TRUE)
}
\keyword{classes}
