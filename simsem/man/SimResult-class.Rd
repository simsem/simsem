\name{SimResult-class}
\Rdversion{1.1}
\docType{class}
\alias{SimResult-class}
\alias{summary,SimResult-method}
\alias{summaryShort,SimResult-method}
\title{Class \code{"SimResult"}: Simulation Result Object}
\description{
This class will save data analysis results from multiple replications, such as fit indices cutoffs or power, parameter values, model misspecification, etc.
}
\section{Objects from the Class}{
Objects can be created by \code{\link{sim}}. 
}
\section{Slots}{
  \describe{
    \item{\code{modelType}:}{Analysis model type (CFA, Path, or SEM) }
    \item{\code{nRep}:}{Number of replications have been created and run simulated data. }
    \item{\code{coef}:}{Parameter estimates from each replication}
    \item{\code{se}:}{Standard errors of parameter estimates from each replication}
    \item{\code{fit}:}{Fit Indices values from each replication}
    \item{\code{converged}:}{The convergence status of each replication: 0 = convergent, 1 = not convergent, 2 = nonconvergent in multiple imputed results, 3 = improper solutions for SE (less than 0 or NA), 4 = improper solution for variance (less than 0), 5 = improper solution for correlation (greater than 1 or less than -1) }
    \item{\code{seed}:}{Seed number. }
	\item{\code{paramValue}:}{Population model underlying each simulated dataset.}
	\item{\code{stdParamValue}:}{Standardized parameters of the population model underlying each simulated dataset.}
	\item{\code{paramOnly}:}{If \code{TRUE}, the result object saves only population characteristics and do not save sample characteristics (e.g., parameter estimates and standard errors.}
	\item{\code{misspecValue}:}{Misspecified-parameter values that are imposed on the population model in each replication.}
	\item{\code{popFit}:}{The amount of population misfit. See details at \code{\link{summaryMisspec}}}
	\item{\code{FMI1}:}{Fraction Missing Method 1.}
	\item{\code{FMI2}:}{Fraction Missing Method 2.}
	\item{\code{cilower}:}{Lower bounds of confidence interval.}
	\item{\code{ciupper}:}{Upper bounds of confidence interval.}
	\item{\code{stdCoef}:}{Standardized coefficients from each replication}
	\item{\code{stdSe}:}{Standard Errors of Standardized coefficients from each replication}
	\item{\code{n}:}{The total sample size of the analyzed data.}
	\item{\code{nobs}:}{The sample size within each group.}
	\item{\code{pmMCAR}:}{Percent missing completely at random.}
	\item{\code{pmMAR}:}{Percent missing at random.}
	\item{\code{extraOut}:}{Extra outputs obtained from running the function specified in \code{outfun} argument in the \code{\link{sim}} function.}
	\item{\code{timing}:}{Time elapsed in each phase of the simulation.}
  }
}
\section{Methods}{
	The following methods are listed alphabetically. More details can be found by following the link of each method.
	\itemize{
		\item \code{\link[=anova,SimResult-method]{anova}} to find the averages of model fit statistics and indices for nested models, as well as the differences of model fit indices among models. This function requires at least two \code{SimResult} objects. 
		\item \code{\link[=coef,SimResult-method]{coef}} to extract parameter estimates of each replication
		\item \code{\link{findCoverage}} to find a value of independent variables (e.g., sample size) that provides a given value of coverage rate.
		\item \code{\link{findPower}} to find a value of independent variables (e.g., sample size) that provides a given value of power of a parameter estimate. 
		\item \code{\link{getCoverage}} to get the coverage rate of the confidence interval of each parameter estimate
		\item \code{\link{getCIwidth}} to get a median or percentile rank (assurance) of confidence interval widths of parameters estimates
		\item \code{\link{getCutoff}} to get the cutoff of fit indices based on a priori alpha level.
		\item \code{\link{getCutoffNested}} to get the cutoff of the difference in fit indices of nested models based on a priori alpha level. 
		\item \code{\link{getCutoffNonNested}} to get the cutoff of the difference in fit indices of nonnested models based on a priori alpha level. 
		\item \code{\link{getExtraOutput}} to get extra outputs that users requested before running a simulation
		\item \code{\link{getPopulation}} to get population parameter values underlying each dataset
		\item \code{\link{getPower}} to get the power of each parameter estimate
		\item \code{\link{getPowerFit}} to get the power in rejecting alternative models based on absolute model fit cutoff. 
		\item \code{\link{getPowerFitNested}} to get the power in rejecting alternative models based on the difference between model fit cutoffs of nested models. 
		\item \code{\link{getPowerFitNonNested}} to get the power in rejecting alternative models based on the difference between model fit cutoffs of nonnested models.  
		\item \code{\link[=inspect,SimResult-method]{inspect}} Extract target information from the simulation result. The available information is listed in this \code{\link[=inspect,SimResult-method]{link}}
		\item \code{\link{likRatioFit}} to find the likelihood ratio (or Bayes factor) based on the bivariate distribution of fit indices
		\item \code{\link{plotCoverage}} to plot the coverage rate of confidence interval of parameter estimates
		\item \code{\link{plotCIwidth}} to plot confidence interval widths with a line of a median or percentile rank (assurance) 
		\item \code{\link{plotCutoff}} to plot sampling distributions of fit indices with an option to draw fit indices cutoffs by specifying a priori alpha level.
		\item \code{\link{plotCutoffNested}} to plot sampling distributions of the difference in fit indices between nested models with an option to draw fit indices cutoffs by specifying a priori alpha level.
		\item \code{\link{plotCutoffNonNested}} to plot sampling distributions of the difference in fit indices between nonnested models with an option to draw fit indices cutoffs by specifying a priori alpha level.
		\item \code{\link{plotMisfit}} to visualize the population misfit and misspecified parameter values
		\item \code{\link{plotPower}} to plot power of parameter estimates 
		\item \code{\link{plotPowerFit}} to plot the power in rejecting alternative models based on absolute model fit cutoff.
		\item \code{\link{plotPowerFitNested}} to plot the power in rejecting alternative models based on the difference between model fit cutoffs of nested models. 
		\item \code{\link{plotPowerFitNonNested}} to plot the power in rejecting alternative models based on the difference between model fit cutoffs of nonnested models.   
		\item \code{\link{pValue}} to find a p-value in comparing sample fit indices with the null sampling distribution of fit indices
		\item \code{\link{pValueNested}} to find a p-value in comparing the difference in sample fit indices between nested models with the null sampling distribution of the difference in fit indices
		\item \code{\link{pValueNonNested}} to find a p-value in comparing the difference in sample fit indices between nonnested models with the null sampling distribution of the difference in fit indices
		\item \code{\link{setPopulation}} to set population model for computing bias
		\item \code{\link{summary}} to summarize the result output
		\item \code{\link{summaryConverge}} to provide a head-to-head comparison between the characteristics of convergent and nonconvergent replications
		\item \code{\link{summaryMisspec}} to provide a summary of model misfit
		\item \code{\link{summaryParam}} to summarize all parameter estimates
		\item \code{\link{summaryPopulation}} to summarize the data generation population underlying the simulation study.
		\item \code{\link{summarySeed}} to provide a summary of the seed number in the simulation
		\item \code{\link{summaryShort}} to provide a short summary of the result output
		\item \code{\link{summaryTime}} to provide a summary of time elapsed in the simulation
	}
}
\author{
    Sunthud Pornprasertmanit (\email{psunthud@gmail.com})
}
\seealso{
	\itemize{
		\item \code{\link{sim}} for the constructor of this class
	}
}
\examples{
showClass("SimResult")
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LY <- bind(loading, 0.7)
RPS <- binds(diag(1))
RTE <- binds(diag(6))
CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA")

# We make the examples running only 5 replications to save time.
# In reality, more replications are needed.
Output <- sim(5, n=500, CFA.Model)

# Summary the simulation result
summary(Output)

# Short summary of the simulation result
summaryShort(Output)

# Find the fit index cutoff
getCutoff(Output, 0.05)

# Summary of parameter estimates
summaryParam(Output)

# Summary of population parameters
summaryPopulation(Output)
}
\keyword{classes}
