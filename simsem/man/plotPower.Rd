\name{plotPower}
\alias{plotPower}
\title{
	Make a power plot of a parameter given varying parameters
}
\description{
	Make a power plot of a parameter given varying parameters (e.g., sample size, percent missing completely at random, or random parameters in the model)
}
\usage{
plotPower(object, powerParam, alpha = 0.05, contParam = NULL, contN = TRUE, 
	contMCAR = TRUE, contMAR = TRUE, useContour=TRUE)
}
\arguments{
  \item{object}{
	\code{\linkS4class{SimResult}} that includes at least one randomly varying parameter (e.g. sample size, percent missing, model parameters)
}
  \item{powerParam}{
	Vector of parameters names that the user wishes to find power for. This can be a vector of names (e.g., "LY1_1", "LY2_2").
}
  \item{alpha}{
	Alpha level to use for power analysis.
}
\item{contParam}{
	Vector of parameters names that vary over replications that users wish to use in the plot.
}
\item{contN}{
	Include the varying sample size in the power plot if available
}
\item{contMCAR}{
	Include the varying MCAR (missing completely at random percentage) in the power plot if available
}
\item{contMAR}{
	Include the varying MAR (missing at random percentage) in the power plot if available
}
\item{useContour}{
	This argument is used when users specify to plot two varying parameters. If \code{TRUE}, the contour plot is used. If \code{FALSE}, perspective plot is used.
}
}
\details{
	Predicting whether each replication is significant or not by varying parameters using logistic regression (without interaction). Then, plot the logistic curves predicting the probability of significance against the target varying parameters.
}
\value{
	Not return any value. This function will plot a graph only.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu}), Alexander M. Schoemann (University of Kansas; \email{schoemann@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimResult}} to see how to create a simResult object with randomly varying parameters.
		\item \code{\link{getPower}} to obtain a statistical power given varying parameters values.
	}
}
\examples{
\dontrun{
# Specify Sample Size by n
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- bind(loading, 0.4)
RPH <- binds(diag(1))
RTD <- binds(diag(6))
CFA.Model <- model(LY = LX, RPS = RPH, RTE = RTD, modelType="CFA")


# Specify both sample size and percent missing completely at random
Output <- sim(NULL, n=seq(100, 200, 20), pmMCAR=c(0, 0.1, 0.2), model=CFA.Model)
plotPower(Output, "1.LY1_1", contMCAR=FALSE)
}
}
