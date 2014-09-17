\name{plotCoverage}
\alias{plotCoverage}
\title{
	Make a plot of confidence interval coverage rates
}
\description{
	Make a plot of confidence interval coverage rates given varying parameters (e.g., sample size, percent missing completely at random, or random parameters in the model)
}
\usage{
plotCoverage(object, coverParam, coverValue = NULL, contParam = NULL, contN = TRUE, 
    contMCAR = TRUE, contMAR = TRUE, useContour = TRUE)
}
\arguments{
  \item{object}{
	\code{\linkS4class{SimResult}} that includes at least one randomly varying parameter (e.g. sample size, percent missing, model parameters)
}
  \item{coverParam}{
	Vector of parameters names that the user wishes to find coverage rate for. This can be a vector of names (e.g., "f1=~y2", "f1~~f2").
}
  \item{coverValue}{
	A target value used that users wish to find the coverage rate of that value (e.g., 0). If \code{NULL}, the parameter values will be used.
}
\item{contParam}{
	Vector of parameters names that vary over replications that users wish to use in the plot.
}
\item{contN}{
	Include the varying sample size in the coverage rate plot if available
}
\item{contMCAR}{
	Include the varying MCAR (missing completely at random percentage) in the coverage rate plot if available
}
\item{contMAR}{
	Include the varying MAR (missing at random percentage) in the coverage rate plot if available
}
\item{useContour}{
	This argument is used when users specify to plot two varying parameters. If \code{TRUE}, the contour plot is used. If \code{FALSE}, perspective plot is used.
}
}
\details{
	Predicting whether the confidence interval of each replication covers target value (or parameter) or not by varying parameters using logistic regression (without interaction). Then, plot the logistic curves predicting the probability of significance against the target varying parameters.
}
\value{
	Not return any value. This function will plot a graph only.
}
\author{
    Sunthud Pornprasertmanit (Texas Tech University; \email{sunthud.pornprasertmanit@ttu.edu}), Alexander M. Schoemann (East Carolina University; \email{schoemanna@ecu.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimResult}} to see how to create a simResult object with randomly varying parameters.
		\item \code{\link{getCoverage}} to obtain a coverage rate given varying parameters values.
	}
}
\examples{
\dontrun{
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LY <- bind(loading, 0.4)
RPS <- binds(diag(1))
RTE <- binds(diag(6))
CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA")

# Specify both continuous sample size and percent missing completely at random. 
# Note that more fine-grained values of n and pmMCAR is needed, e.g., n=seq(50, 500, 1) 
# and pmMCAR=seq(0, 0.2, 0.01)

Output <- sim(NULL, n=seq(100, 200, 20), pmMCAR=c(0, 0.1, 0.2), model=CFA.Model)

# Plot the power of the first factor loading along the sample size value
plotCoverage(Output, "f1=~y1", contMCAR=FALSE)
plotCoverage(Output, "f1=~y1", coverValue = 0, contMCAR=FALSE)

# Plot the power of the correlation along the sample size and percent missing completely at random
plotCoverage(Output, "f1=~y1")
}
}
