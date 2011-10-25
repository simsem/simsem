\name{simAnalysis-class}
\Rdversion{1.1}
\docType{class}
\alias{simAnalysis-class}
\alias{summary,simAnalysis-method}
\title{Class \code{"simAnalysis"}}
\description{
This class will save the analysis results from a single analysis.
}
\section{Objects from the Class}{
Objects can be created by \code{\link{run}} on the \code{\linkS4class{simModel}}. It can also be called from the form \code{new("simAnalysis", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{Parameters}:}{Set of all free parameters and values of fixed parameters in the model.}
    \item{\code{Starting.Values}:}{All starting values of free parameters}
    \item{\code{Constraint}:}{Equality constraints in \code{\linkS4class{simConstraint}} class }
    \item{\code{Program}:}{Packages used in data analysis, either \code{lavaan} or \code{OpenMx}. The default is \code{lavaan} }
    \item{\code{Estimates}:}{Parameter estimates saved in matrix arrangement}
    \item{\code{SE}:}{Standard errors of parameter saved in matrix arrangement}
    \item{\code{Fit}:}{Fit Indices values from each replication}
    \item{\code{Convergence}:}{Number of convergence replications }
  }
}
\section{Methods}{
	\enumerate{
		\item{summary}{To summarize the object}
	}
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\enumerate{
		\item \code{\linkS4class{simModel}} for analysis model
	}
}
\examples{
showClass("simResult")
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- matrix.object(loading, 0.7)
PH <- sym.matrix.object(diag(1))
TD <- sym.matrix.object(diag(6))
CFA.Model <- matrix.CFA.object(LY = LX, PS = PH, TE = TD)
SimData <- data.object(500, CFA.Model)
SimModel <- model.object(CFA.Model)
Data <- run(SimData)
Result <- run(SimModel, Data)
summary(Result)
}
\keyword{classes}
