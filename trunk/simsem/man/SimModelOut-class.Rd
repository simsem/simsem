\name{SimModelOut-class}
\Rdversion{1.1}
\docType{class}
\alias{SimModelOut-class}
\alias{summary,SimModelOut-method}
\title{Class \code{"SimModelOut"}}
\description{
This class will save the analysis results from a single analysis.
}
\section{Objects from the Class}{
Objects can be created by \code{\link{run}} on the \code{\linkS4class{SimModel}}. It can also be called from the form \code{new("SimModelOut", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{param}:}{Set of all free parameters and values of fixed parameters in the model.}
    \item{\code{start}:}{All starting values of free parameters}
    \item{\code{equalCon}:}{Equality constraints in \code{\linkS4class{SimEqualCon}} class }
    \item{\code{package}:}{Packages used in data analysis, either \code{lavaan} or \code{OpenMx}. The default is \code{lavaan} }
    \item{\code{coef}:}{Parameter estimates saved in matrix arrangement}
    \item{\code{se}:}{Standard errors of parameter saved in matrix arrangement}
    \item{\code{fit}:}{Fit Indices values from each replication}
    \item{\code{converged}:}{Number of convergence replications }
	\item{\code{paramValue}:}{The parameter values behind the analyzed data.}
  }
}
\section{Methods}{
	\itemize{
		\item{summary}{To summarize the object}
		\item{summaryParam}{To summarize only parameter estimates, standard errors, and significance}
	}
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimModel}} for analysis model
	}
}
\examples{
showClass("SimResult")
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- simMatrix(loading, 0.7)
PH <- symMatrix(diag(1))
TD <- symMatrix(diag(6))
CFA.Model <- simSetCFA(LY = LX, PS = PH, TE = TD)
SimData <- simData(500, CFA.Model)
SimModel <- simModel(CFA.Model)
Data <- run(SimData)
Result <- run(SimModel, Data)
summary(Result)
}
\keyword{classes}
