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
    \item{\code{Parameters}:}{Set of all free parameters and values of fixed parameters in the model.}
    \item{\code{Starting.Values}:}{All starting values of free parameters}
    \item{\code{Constraint}:}{Equality constraints in \code{\linkS4class{SimEqualCon}} class }
    \item{\code{Program}:}{Packages used in data analysis, either \code{lavaan} or \code{OpenMx}. The default is \code{lavaan} }
  }
}
\section{Methods}{
  \describe{
    \item{run}{Analyze data. The first argument is the \code{SimModel} and the second argument is data saved in \code{data.frame} }
	\item{summary}{To summarize the object}
	}
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\enumerate{
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
