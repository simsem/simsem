\name{continuousPower}
\alias{continuousPower}
\title{
	Find power of model parameters when simulations have randomly varying parameters
}
\description{
	A function to find the power of  parameters in a model when one or more of the simulations parameters vary randomly across replications.
}
\usage{
continuousPower(simResult, contN = TRUE, contMCAR = FALSE, contMAR = FALSE, 
	contParam = NULL, alpha = .05, powerParam = NULL, pred = NULL)
}
\arguments{
  \item{simResult}{
	\code{\link{SimResult}} that includes at least one randomly varying parameter (e.g. sample size, percent missing, model parameters)
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
\item{alpha}{
	Alpha level to use for power analysis.
}
\item{powerParam}{
	Vector of parameters names that the user wishes to find power for. This can be a vector of names (e.g., "LY1_1", "LY2_2"), or the name of a matrix (e.g. "PS"), if the name of a matrix is used power for all parameters in that matrix will be returned. If parameters are not specified, power for all parameters in the model will be returned.
}
\item{pred}{
	A list of varying parameter values that users wish to find statistical power from.
}
}
\details{
	A common use of simulations is to conduct power analyses, especially when using SEM (Muthen & Muthen, 2002). Here, researchers select values for each parameter and a sample size and run a simulation to determine power in those conditions (the proportion of generated datasets in which a particular parameter of interest is significantly different from zero). To evaluate power at multiple sample sizes, one simulation for each sample size must be run. By continuously varying sample size across replications, only a single simulation is needed. In this simulation, the sample size for each replication varies randomly across plausible sample sizes (e.g., sample sizes between 200 and 500). For each replication, the sample size and significance of each parameter (0 = not significant, 1 = significant) are recorded. When the simulation is complete, parameter significance is regressed on sample size using logistic regression. For a given sample size, the predicted probability from the logistic regression equation is the power to detect an effect at that sample size. This approach can be extended to other randomly varying simulation parameters such as the percentage of missing data, and model parameters.
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
		\item \code{\link{SimResult}} to see how to create a simResult object with randomly varying parameters.
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
dat <- generate(CFA.Model, 50)
out <- analyze(CFA.Model, dat)

# Specify both continuous sample size and percent missing completely at random. Note that more fine-grained 
# values of n and pmMCAR is needed, e.g., n=seq(50, 500, 1) and pmMCAR=seq(0, 0.2, 0.01)
Output <- sim(NULL, CFA.Model, n=seq(100, 200, 20), pmMCAR=c(0, 0.1, 0.2))
summary(Output)

# Find the power of all combinations of different sample size and percent MCAR missing
Cpow <- continuousPower(Output, contN = TRUE, contMCAR = TRUE)
Cpow

# Find the power of parameter estimates when sample size is 200 and percent MCAR missing is 0.3
Cpow2 <- continuousPower(Output, contN = TRUE, contMCAR = TRUE, pred=list(N = 200, pmMCAR = 0.3))
Cpow2
}
}
