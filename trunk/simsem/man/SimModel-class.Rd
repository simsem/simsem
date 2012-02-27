\name{SimModel-class}
\Rdversion{1.1}
\docType{class}
\alias{SimModel-class}
\alias{run,SimModel-method}
\alias{summary,SimModel-method}

\title{Class \code{"SimModel"}}
\description{
This class will save information for analysis model and be ready for data analysis.
}
\section{Objects from the Class}{
Objects can be created by \code{\link{simModel}}. It can also be called by \code{new("SimModel", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{modelType}:}{Model type (CFA, Path, or SEM)}
    \item{\code{param}:}{Set of all free parameters and values of fixed parameters in the model.}
    \item{\code{start}:}{All starting values of free parameters}
    \item{\code{equalCon}:}{Equality constraints in \code{\linkS4class{SimEqualCon}} class }
    \item{\code{package}:}{Packages used in data analysis, either \code{lavaan} or \code{OpenMx}. The default is \code{lavaan} }
    \item{\code{estimator}:}{The default is \code{ML} estimator. Other alternatives are \code{GLS}, \code{WLS}, \code{MLM}, \code{MLF}, and \code{MLR}. Check the sem function help file in the \code{lavaan} package for further details}
  }
}
\section{Methods}{
  \describe{
    \item{run}{To analyze data. There are two required arguments: \code{object} and \code{data}. \code{object} is the \code{SimModel} object. \code{data} is data saved in \code{data.frame}. The following arguments are not required. \code{simMissing}, is the \code{SimMissing} object. \code{indicatorLab} is the labels of the \code{data} used to name the indicators in the model. If x-side is specified, the x side goes first then the y side. \code{factorLab} is the labels of the factors in the model. If x-side is specified, the x side goes first and then the y side. }
	\item{summary}{To summarize the object}
	}
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\link{simModel}} for the constructor of this class.
		\item \code{\linkS4class{SimEqualCon}} for specifying equality constraints.	
	}
}
\examples{
showClass("SimModel")
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
summary(SimModel)
}
\keyword{classes}
