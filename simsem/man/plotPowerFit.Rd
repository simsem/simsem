\name{plotPowerFit}
\alias{plotPowerFit}
\title{
Plot sampling distributions of fit indices that visualize power of rejecting datasets underlying misspecified models
}
\description{
This function will plot sampling distributions of fit indices that visualize power in rejecting the misspecified models
}
\usage{
plotPowerFit(altObject, nullObject = NULL, cutoff = NULL, usedFit = NULL, 
alpha = 0.05, contN = TRUE, contMCAR = TRUE, contMAR = TRUE, 
useContour = TRUE, logistic = TRUE)
}
\arguments{
  \item{altObject}{
	The result object (\code{\linkS4class{SimResult}}) saves the simulation result of fitting the hypothesized model when the hypothesized model is \code{FALSE}.
}
  \item{nullObject}{
	The result object (\code{\linkS4class{SimResult}}) saves the simulation result of fitting the hypothesized model when the hypothesized model is \code{TRUE}. This argument may be not specified if the \code{cutoff} is specified.
}
  \item{cutoff}{
	A vector of priori cutoffs for fit indices.
}
  \item{usedFit}{
	Vector of names of fit indices that researchers wish to plot.
}
  \item{alpha}{
	A priori alpha level
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
	If there are two of sample size, percent completely at random, and percent missing at random are varying, the \code{plotCutoff} function will provide 3D graph. Contour graph is a default. However, if this is specified as \code{FALSE}, perspective plot is used.
}
  \item{logistic}{
	If \code{logistic} is \code{TRUE} and the varying parameter exists (e.g., sample size or percent missing), the plot based on logistic regression predicting the significance by the varying parameters is preferred. If \code{FALSE}, the overlaying scatterplot with a line of cutoff is plotted.
}
}
\value{
NONE. Only plot the fit indices distributions.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimResult}} for simResult that used in this function.
		\item \code{\link{getCutoff}} to find values of cutoffs based on null hypothesis sampling distributions only
		\item \code{\link{getPowerFit}} to find power of rejecting the hypothesized model when the hypothesized model is \code{FALSE}.
	}
}
\examples{
\dontrun{
# Null model: One factor model
loading.null <- matrix(0, 6, 1)
loading.null[1:6, 1] <- NA
LY.NULL <- bind(loading.null, 0.7)
RPS.NULL <- binds(diag(1))
RTE <- binds(diag(6))
CFA.Model.NULL <- model(LY = LY.NULL, RPS = RPS.NULL, RTE = RTE, modelType="CFA")

# We make the examples running only 5 replications to save time.
# In reality, more replications are needed.
Output.NULL <- sim(50, n=50, model=CFA.Model.NULL, generate=CFA.Model.NULL) 

# Alternative model: Two-factor model
loading.alt <- matrix(0, 6, 2)
loading.alt[1:3, 1] <- NA
loading.alt[4:6, 2] <- NA
LY.ALT <- bind(loading.alt, 0.7)
latent.cor.alt <- matrix(NA, 2, 2)
diag(latent.cor.alt) <- 1
RPS.ALT <- binds(latent.cor.alt, 0.5)
CFA.Model.ALT <- model(LY = LY.ALT, RPS = RPS.ALT, RTE = RTE, modelType="CFA")
Output.ALT <- sim(50, n=50, model=CFA.Model.NULL, generate=CFA.Model.ALT)

# Plot the power based on derived cutoff from the null model using four fit indices
plotPowerFit(Output.ALT, nullObject=Output.NULL, alpha=0.05, 
	usedFit=c("RMSEA", "CFI", "TLI", "SRMR"))

# Plot the power of rejecting null model when the rule of thumb from Hu & Bentler (1999) is used
Rule.of.thumb <- c(RMSEA=0.05, CFI=0.95, TLI=0.95, SRMR=0.06)
plotPowerFit(Output.ALT, cutoff=Rule.of.thumb, alpha=0.05, 
	usedFit=c("RMSEA", "CFI", "TLI", "SRMR"))

# The example of continous varying sample size. Note that more fine-grained 
# values of n is needed, e.g., n=seq(50, 500, 1)
Output.NULL2 <- sim(NULL, n=seq(50, 250, 25), model=CFA.Model.NULL, generate=CFA.Model.NULL)
Output.ALT2 <- sim(NULL, n=seq(50, 250, 25), model=CFA.Model.NULL, generate=CFA.Model.ALT)

# Plot the power based on derived cutoff from the null model using four fit indices 
# along sample size
plotPowerFit(Output.ALT2, nullObject=Output.NULL2, alpha=0.05, 
	usedFit=c("RMSEA", "CFI", "TLI", "SRMR"))

# Plot the power based on rule of thumb along sample size
plotPowerFit(Output.ALT2, cutoff=Rule.of.thumb, alpha=0.05, 
	usedFit=c("RMSEA", "CFI", "TLI", "SRMR"))
}
}
