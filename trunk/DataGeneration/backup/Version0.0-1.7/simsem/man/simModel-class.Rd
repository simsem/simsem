\name{simModel-class}
\Rdversion{1.1}
\docType{class}
\alias{simModel-class}
\alias{run,simModel-method}

\title{Class \code{"simModel"}}
\description{
This class will save information for analysis model and be ready for data analysis.
}
\section{Objects from the Class}{
Objects can be created by \code{\link{model.object}}. It can also be called by \code{new("simModel", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{Tag}:}{Model type (CFA, Path, or SEM)}
    \item{\code{Parameters}:}{Set of all free parameters and values of fixed parameters in the model.}
    \item{\code{Starting.Values}:}{All starting values of free parameters}
    \item{\code{Constraint}:}{Equality constraints in \code{\linkS4class{simConstraint}} class }
    \item{\code{Program}:}{Packages used in data analysis, either \code{lavaan} or \code{OpenMx}. The default is \code{lavaan} }
  }
}
\section{Methods}{
  \describe{
    \item{run}{\code{signature(object = "simModel")}: ... }
	 }
	 %Waiting for summary function
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\enumerate{
		\item \code{\link{model.object}} for the constructor of this class.
		\item \code{\linkS4class{simConstraint}} for specifying equality constraints.	
	}
}
\examples{
showClass("simModel")
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- matrix.object(loading, loadingValues)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
PH <- sym.matrix.object(latent.cor, 0.5)
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
TD <- sym.matrix.object(error.cor)
CFA.Model <- matrix.CFA.object(LX = LX, PH = PH, TD = TD)
SimModel <- model.object(CFA.Model)
#summary(SimModel)
}
\keyword{classes}
