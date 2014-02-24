\name{continuousCoverage}
\alias{continuousCoverage}
\title{
	Find coverage rate of model parameters when simulations have randomly varying parameters
}
\description{
	A function to find the coverage rate of confidence intervals in a model when one or more of the simulations parameters vary randomly across replications.
}
\usage{
continuousCoverage(simResult, coverValue = NULL, contN = TRUE, contMCAR = FALSE, 
    contMAR = FALSE, contParam = NULL, coverParam = NULL, pred = NULL)
}
\arguments{
  \item{simResult}{
	\code{\linkS4class{SimResult}} that includes at least one randomly varying parameter (e.g. sample size, percent missing, model parameters)
}
  \item{coverValue}{
	A target value used that users wish to find the coverage rate of that value (e.g., 0). If \code{NULL}, the parameter values will be used.
}
  \item{contN}{
	Logical indicating if N varies over replications.
}
  \item{contMCAR}{
	Logical indicating if the percentage of missing data that is MCAR varies over replications.
}
\item{contMAR}{
	Logical indicating if the percentage of missing data that is MAR varies over replications.
}
\item{contParam}{
	Vector of parameters names that vary over replications.
}
\item{coverParam}{
	Vector of parameters names that the user wishes to find coverage rate for. This can be a vector of names (e.g., "f1=~y2", "f1~~f2"). If parameters are not specified, coverage rates for all parameters in the model will be returned.
}
\item{pred}{
	A list of varying parameter values that users wish to find statistical power from.
}
}
\details{
	In this function, the coverage (which can be 0 or 1) is regressed on randomly varying simulation parameters (e.g., sample size, percentage of missing data, or model parameters) using logistic regression. For a set of independent variables values, the predicted probability from the logistic regression equation is the predicted coverage rate.
}
\value{
	Data frame containing columns representing values of the randomly varying simulation parameters, and coverage rates for model parameters of interest.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu}), Alexander M. Schoemann (University of Kansas; \email{schoemann@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimResult}} to see how to create a simResult object with randomly varying parameters.
	}
}
\examples{
\dontrun{
# Specify Sample Size by n
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LY <- bind(loading, 0.7)
RPS <- binds(diag(1))
RTE <- binds(diag(6))
CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA")

# Specify both continuous sample size and percent missing completely at random. 
# Note that more fine-grained values of n and pmMCAR is needed, e.g., n=seq(50, 500, 1) 
# and pmMCAR=seq(0, 0.2, 0.01)
Output <- sim(NULL, CFA.Model, n=seq(100, 200, 20), pmMCAR=c(0, 0.1, 0.2))
summary(Output)

# Find the coverage rates of all combinations of different sample size and percent MCAR missing
Ccover <- continuousCoverage(Output, contN = TRUE, contMCAR = TRUE)
Ccover

# Find the coverage rates of parameter estimates when sample size is 200 
# and percent MCAR missing is 0.3
Ccover2 <- continuousCoverage(Output, coverValue=0, contN = TRUE, contMCAR = TRUE, 
     pred=list(N = 200, pmMCAR = 0.3))
Ccover2
}
}
