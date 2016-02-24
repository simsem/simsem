\name{getCoverage}
\alias{getCoverage}
\title{
	Find coverage rate of model parameters 
}
\description{
	A function to find the coverage rate of confidence intervals in a model when none, one, or more of the simulations parameters vary randomly across replications.
}
\usage{
getCoverage(simResult, coverValue = NULL, contParam = NULL, coverParam = NULL, 
    nVal = NULL, pmMCARval = NULL, pmMARval = NULL, paramVal = NULL)
}
\arguments{
  \item{simResult}{
	\code{\linkS4class{SimResult}} that may include randomly varying parameters (e.g. sample size, percent missing, model parameters)
}
  \item{coverValue}{
	A target value used that users wish to find the coverage rate of that value (e.g., 0). If \code{NULL}, the parameter values will be used.
}
\item{contParam}{
	Vector of parameters names that vary over replications.
}
\item{coverParam}{
	Vector of parameters names that the user wishes to find coverage rate for. This can be a vector of names (e.g., "f1=~y2", "f1~~f2"). If parameters are not specified, coverage rates for all parameters in the model will be returned.
}
\item{nVal}{
	The sample size values that users wish to find power from.
}
\item{pmMCARval}{
	The percent completely missing at random values that users wish to find power from.
}
\item{pmMARval}{
	The percent missing at random values that users wish to find power from.
}
\item{paramVal}{
	A list of varying parameter values that users wish to find power from.
}
}
\details{
	In this function, the coverage (which can be 0 or 1) is regressed on randomly varying simulation parameters (e.g., sample size, percentage of missing data, or model parameters) using logistic regression. For a set of independent variables values, the predicted probability from the logistic regression equation is the predicted coverage rate.
}
\value{
	Data frame containing columns representing values of the randomly varying simulation parameters, and coverage rates for model parameters of interest.
}
\author{
    Sunthud Pornprasertmanit (\email{psunthud@gmail.com}), Alexander M. Schoemann (East Carolina University; \email{schoemanna@ecu.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimResult}} to see how to create a simResult object with randomly varying parameters.
	}
}
\examples{
\dontrun{
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LY <- bind(loading, 0.7)
RPS <- binds(diag(1))
RTE <- binds(diag(6))
CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA")

# Specify both sample size and percent missing completely at random. Note that more fine-grained 
# values of n and pmMCAR is needed, e.g., n=seq(50, 500, 1) and pmMCAR=seq(0, 0.2, 0.01)
Output <- sim(NULL, model=CFA.Model, n=seq(100, 200, 20), pmMCAR=c(0, 0.1, 0.2))
summary(Output)

# Get the coverage rates of all possible combinations of n and pmMCAR
getCoverage(Output)

# Get the coverage rates of the combinations of n of 100 and 200 and pmMCAR of 0, 0.1, and 0.2
getCoverage(Output, coverValue = 0, nVal=c(100, 200), pmMCARval=c(0, 0.1, 0.2))
}
}
