\name{SimResult-class}
\Rdversion{1.1}
\docType{class}
\alias{SimResult-class}
\alias{summary,SimResult-method}
\alias{summaryPopulation,SimResult-method}
\alias{getPopulation,SimResult-method}
\alias{setPopulation,SimResult,data.frame-method}
\alias{setPopulation,SimResult,SimSet-method}
\alias{setPopulation,SimResult,VirtualRSet-method}
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
	\item{\code{n}:}{Sample size of the analyzed data.}
	\item{\code{pmMCAR}:}{Percent missing completely at random.}
	\item{\code{pmMAR}:}{Percent missing at random.}
  }
}
\section{Methods}{
	\itemize{
		\item \code{\link{getCutoff}} to getCutoff of fit indices based on a priori alpha level.
		\item \code{\link{getPowerFit}} to getPowerFit of rejection when the simResult is the alternative hypothesis and users specify cutoffs of the fit indices.
		\item \code{\link{plotCutoff}} to plot null hypothesis sampling distributions of fit indices with an option to draw fit indices cutoffs by specifying a priori alpha level.
		\item \code{\link{plotPowerFit}} to plot alternative hypothesis (and null hypothesis) with a priori cutoffs or alpha level.
		\item \code{\link{summary}} to summarize the result output
		\item \code{\link{summaryParam}} to summarize all parameter estimates
		\item \code{anova} find the averages of model fit statistics and indices for nested models, as well as the differences of model fit indices among models. This function requires at least two \code{SimResult} objects. See \code{\link{anova}} for further details.
		\item \code{\link{summaryPopulation}} to summarize the data generation population underlying the simulation study.
		\item \code{\link{getPopulation}} to extract the data generation population underlying the simulation study. This method will return a data frame of the population underlying each replication.
		\item \code{\link{setPopulation}} to put the appropriate data generation model into the result object. If the appropriate data generation model is put (the same model as the analysis model), the bias in parameter estimates and standard errors will be able to be calculated by the \code{summary} function. The first argument is the result object. The second argument can be either \code{data.frame} of the population or \code{SimSet} of the population. See the 'modeling with covariate' in the manual for an example.	
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
SimData <- simData(CFA.Model, 500)
SimModel <- simModel(CFA.Model)
# We make the examples running only 5 replications to save time.
# In reality, more replications are needed.
Output <- simResult(5, SimData, SimModel)
summary(Output)
getCutoff(Output, 0.05)
summaryParam(Output)
summaryPopulation(Output)
param <- getPopulation(Output)
Output <- setPopulation(Output, param)
Output2 <- setPopulation(Output, CFA.Model)
}
\keyword{classes}
