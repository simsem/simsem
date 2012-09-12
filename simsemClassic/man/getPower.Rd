\name{getPower}
\alias{getPower}
\title{
	Find power of model parameters 
}
\description{
	A function to find the power of  parameters in a model when none, one, or more of the simulations parameters vary randomly across replications.
}
\usage{
getPower(simResult, alpha = 0.05, contParam = NULL, powerParam = NULL, 
	nVal = NULL, pmMCARval = NULL, pmMARval = NULL, paramVal = NULL)
}
\arguments{
  \item{simResult}{
	\code{\linkS4class{SimResult}} that may include at least one randomly varying parameter (e.g. sample size, percent missing, model parameters)
}
  \item{alpha}{
	Alpha level to use for power analysis.
}
\item{contParam}{
	Vector of parameters names that vary over replications.
}
\item{powerParam}{
	Vector of parameters names that the user wishes to find power for. This can be a vector of names (e.g., "LY1_1", "LY2_2"), or the name of a matrix (e.g. "PS"), if the name of a matrix is used power for all parameters in that matrix will be returned. If parameters are not specified, power for all parameters in the model will be returned.
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
	A common use of simulations is to conduct power analyses, especially when using SEM (Muthen & Muthen, 2002). Here, researchers could select values for each parameter and a sample size and run a simulation to determine power in those conditions (the proportion of generated datasets in which a particular parameter of interest is significantly different from zero). To evaluate power at multiple sample sizes, one simulation for each sample size must be run. This function not only calculate power for each sample size but also calculate power for multiple sample sizes varying continuously. By continuously varying sample size across replications, only a single simulation is needed. In this simulation, the sample size for each replication varies randomly across plausible sample sizes (e.g., sample sizes between 200 and 500). For each replication, the sample size and significance of each parameter (0 = not significant, 1 = significant) are recorded. When the simulation is complete, parameter significance is regressed on sample size using logistic regression. For a given sample size, the predicted probability from the logistic regression equation is the power to detect an effect at that sample size. This approach can be extended to other randomly varying simulation parameters such as the percentage of missing data, and model parameters.
}
\value{
	Data frame containing columns representing values of the randomly varying simulation parameters, and power for model parameters of interest.
}
\references{
Muthen, L. K., & Muthen, B. O. (2002). How to use a Monte Carlo study to decide on sample size and determine power. \emph{Structural Equation Modeling, 4,} 599-620.
}
\author{
    Alexander M. Schoemann (University of Kansas; \email{schoemann@ku.edu}), Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
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
LX <- simMatrix(loading, 0.7)
RPH <- symMatrix(diag(1))
RTD <- symMatrix(diag(6))
CFA.Model <- simSetCFA(LY = LX, RPS = RPH, RTE = RTD)
SimData <- simData(CFA.Model, 500)
SimModel <- simModel(CFA.Model)
# We will use only 5 replications to save time.
# In reality, more replications are needed.

# Specify both sample size and percent missing completely at random
Output <- simResult(NULL, SimData, SimModel, n=seq(100, 200, 20), pmMCAR=c(0, 0.1, 0.2))
summary(Output)

getPower(Output)

getPower(Output, nVal=c(100, 200), pmMCARval=c(0, 0.1, 0.2))
}
}
