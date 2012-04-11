\name{SimModelOut-class}
\Rdversion{1.1}
\docType{class}
\alias{SimModelOut-class}
\alias{summary,SimModelOut-method}
\alias{createImpliedMACS,SimModelOut-method}
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
	\item{\code{n}:}{Sample size of the analyzed data.}
  }
}
\section{Methods}{
	\itemize{
		\item \code{\link{summary}} To summarize the object
		\item \code{\link{summaryParam}} To summarize only parameter estimates, standard errors, and significance
		\item \code{\link{createImpliedMACS}} To create the model implied means and covariance matrix from the parameter estimates
		\item \code{anova} find the averages of model fit statistics and indices for nested models, as well as the differences of model fit indices among models. This function requires at least two \code{SimModelOut} objects. See \code{\link{anova}} for further details.
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
showClass("SimModelOut")
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- simMatrix(loading, 0.7)
RPH <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model <- simSetCFA(LY = LX, RPS = RPH, RTE = RTD)
SimData <- simData(CFA.Model, 500)
SimModel <- simModel(CFA.Model)
Data <- run(SimData)
Result <- run(SimModel, Data)
summary(Result)
}
\keyword{classes}
