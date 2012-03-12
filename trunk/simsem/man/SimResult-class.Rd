\name{SimResult-class}
\Rdversion{1.1}
\docType{class}
\alias{SimResult-class}
\alias{summary,SimResult-method}

\title{Class \code{"SimResult"}}
\description{
This class will save data analysis results from multiple replications and ready to find some useful statistics, such as fit indices cutoffs or power.
}
\section{Objects from the Class}{
Objects can be created by \code{\link{simResult}}. It can also be called from the form \code{new("SimResult", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{modelType}:}{Analysis model type (CFA, Path, or SEM) }
    \item{\code{nRep}:}{Number of replications have been created and run simulated data. }
    \item{\code{coef}:}{Parameter estimates from each replication}
    \item{\code{se}:}{Standard errors of parameter estimates from each replication}
    \item{\code{fit}:}{Fit Indices values from each replication}
    \item{\code{converged}:}{Number of convergence replications }
    \item{\code{seed}:}{Seed number. }
	\item{\code{paramValue}:}{Population model underlying each simulated dataset.}
	\item{\code{FMI1}:}{Fraction Missing Method 1.}
	\item{\code{FMI2}:}{Fraction Missing Method 2.}
	\item{\code{stdCoef}:}{Standardized coefficients from each replication}
  }
}
\section{Methods}{
	\itemize{
		\item \code{getCutoff} to getCutoff of fit indices based on a priori alpha level.
		\item \code{getPower} to getPower of rejection when the simResult is the alternative hypothesis and users specify cutoffs of the fit indices.
		\item \code{plotCutoff} to plot null hypothesis sampling distributions of fit indices with an option to draw fit indices cutoffs by specifying a priori alpha level.
		\item \code{plotPower} to plot alternative hypothesis (and null hypothesis) with a priori cutoffs or alpha level.
		\item \code{summary} to summarize the result output
		\item \code{summaryParam} to summarize all parameter estimates
		\item \code{anova} find the averages of model fit statistics and indices for nested models, as well as the differences of model fit indices among models. This function requires at least two \code{SimResult} objects. See \code{\link{anova}} for further details.
	}
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimData}} for data generation model.
		\item \code{\linkS4class{SimModel}} for analysis model
		\item \code{\link{simResult}} for the constructor of this class
	
	}
}
\examples{
showClass("SimResult")
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- simMatrix(loading, 0.7)
RPH <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model <- simSetCFA(LY = LX, RPS = RPH, RTE = RTD)
SimData <- simData(500, CFA.Model)
SimModel <- simModel(CFA.Model)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
Output <- simResult(50, SimData, SimModel)
summary(Output)
getCutoff(Output, 0.05)
}
\keyword{classes}
