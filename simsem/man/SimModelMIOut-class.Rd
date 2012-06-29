\name{SimModelMIOut-class}
\Rdversion{1.1}
\docType{class}
\alias{SimModelMIOut-class}
\title{Class \code{"SimModelMIOut"}}
\description{
This class will save the analysis results from a single analysis using multiple imputation.
}
\section{Objects from the Class}{
Objects can be created by \code{\link{run}} on the \code{\linkS4class{SimModel}} using multiple imputation. It can also be called from the form \code{new("SimModelMIOut", ...)}.
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
	\item{\code{FMI1}:}{The fraction missing method 1.}
	\item{\code{FMI2}:}{The fraction missing method 2.}
	\item{\code{n}:}{Sample size of the analyzed data.}
  }
}
\section{Methods}{
	\itemize{
		\item \code{\link{summary}} To summarize the object
		\item \code{\link{summaryParam}} To summarize only parameter estimates, standard errors, and significance
		\item \code{anova} find the averages of model fit statistics and indices for nested models, as well as the differences of model fit indices among models. This function requires at least two \code{SimModelMIOut} objects. See \code{\link{anova}} for further details.
	}
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimModel}} for analysis model
		\item \code{\linkS4class{SimModelOut}} for the original output model
	}
}
\examples{
showClass("SimModelMIOut")
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- simMatrix(loading, 0.7)
RPH <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model <- simSetCFA(LY = LX, RPS = RPH, RTE = RTD)
SimData <- simData(CFA.Model, 500)
SimModel <- simModel(CFA.Model)
SimMissing <- simMissing(pmMCAR=0.05, numImps=5)
Data <- run(SimData)
Data <- run(SimMissing, Data)
Result <- run(SimModel, Data, SimMissing)
summary(Result)
}
\keyword{classes}
