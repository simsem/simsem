\name{SimDataOut-class}
\Rdversion{1.1}
\docType{class}
\alias{SimDataOut-class}
\alias{summary,SimDataOut-method}
\title{Class \code{"SimDataOut"}}
\description{
This class will provide the simulated dataset and population behind the generated dataset.
}
\section{Objects from the Class}{
Objects can be created by \code{\link{run}} on the \code{\linkS4class{SimData}} with \code{dataOnly=FASE}. It can also be called from the form \code{new("SimDataOut", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{modelType}:}{Analysis model type (CFA, Path, or SEM)}
    \item{\code{data}:}{The simulated data}
    \item{\code{param}:}{Model specification that used in data generation. It must be in \code{\linkS4class{SimSet}} class.}
    \item{\code{paramOut}:}{Parameter values underlying the simulated data.}
    \item{\code{misspecOut}:}{Model misspecification underlying the simulated data}
    \item{\code{equalCon}:}{Equality constraints in data generation. It must be in \code{\linkS4class{SimEqualCon}} class.}
  }
}
\section{Methods}{
	\itemize{
		\item{summary}{To summarize the object}
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
PH <- symMatrix(diag(1))
TD <- symMatrix(diag(6))
CFA.Model <- simSetCFA(LY = LX, PS = PH, TE = TD)
SimData <- simData(500, CFA.Model)
SimModel <- simModel(CFA.Model)
Data <- run(SimData, dataOnly=FALSE)
Result <- run(SimModel, Data)
summary(Data)
}
\keyword{classes}
