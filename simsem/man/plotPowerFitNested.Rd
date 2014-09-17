\name{plotPowerFitNested}
\alias{plotPowerFitNested}
\title{
	Plot power of rejecting a nested model in a nested model comparison by each fit index 
}
\description{
	This function will plot sampling distributions of the differences in fit indices between parent and nested models. Two sampling distributions will be compared: nested model is \code{FALSE} (alternative model) and nested model is \code{TRUE} (null model). 
}
\usage{
plotPowerFitNested(altNested, altParent, nullNested = NULL, 
nullParent = NULL, cutoff = NULL, usedFit = NULL, alpha = 0.05, 
contN = TRUE, contMCAR = TRUE, contMAR = TRUE, useContour = TRUE, 
logistic = TRUE)
}
\arguments{
  \item{altNested}{
	\code{\linkS4class{SimResult}} that saves the simulation result of the nested model when the nested model is \code{FALSE}.
}
  \item{altParent}{
	\code{\linkS4class{SimResult}} that saves the simulation result of the parent model when the nested model is \code{FALSE}.
}
  \item{nullNested}{
	\code{\linkS4class{SimResult}} that saves the simulation result of the nested model when the nested model is \code{TRUE}. This argument may not be specified if the \code{cutoff} is specified.
}
  \item{nullParent}{
	\code{\linkS4class{SimResult}} that saves the simulation result of the parent model when the nested model is \code{TRUE}. This argument may not be specified if the \code{cutoff} is specified.
}
  \item{cutoff}{
	A vector of priori cutoffs for the differences in fit indices.
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
    Sunthud Pornprasertmanit (Texas Tech University; \email{sunthud.pornprasertmanit@ttu.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimResult}} for simResult that used in this function.
		\item \code{\link{getCutoffNested}} to find the cutoffs of the differences in fit indices
		\item \code{\link{plotCutoffNested}} to visualize the cutoffs of the differences in fit indices
		\item \code{\link{getPowerFitNested}} to find the power in rejecting the nested model by the difference in fit indices cutoffs
	}
}
\examples{
\dontrun{
# Null model: One-factor model
loading.null <- matrix(0, 6, 1)
loading.null[1:6, 1] <- NA
LY.NULL <- bind(loading.null, 0.7)
RPS.NULL <- binds(diag(1))
RTE <- binds(diag(6))
CFA.Model.NULL <- model(LY = LY.NULL, RPS = RPS.NULL, RTE = RTE, modelType="CFA")

# Alternative model: Two-factor model
loading.alt <- matrix(0, 6, 2)
loading.alt[1:3, 1] <- NA
loading.alt[4:6, 2] <- NA
LY.ALT <- bind(loading.alt, 0.7)
latent.cor.alt <- matrix(NA, 2, 2)
diag(latent.cor.alt) <- 1
RPS.ALT <- binds(latent.cor.alt, 0.7)
CFA.Model.ALT <- model(LY = LY.ALT, RPS = RPS.ALT, RTE = RTE, modelType="CFA")

# In reality, more than 10 replications are needed
Output.NULL.NULL <- sim(10, n=500, model=CFA.Model.NULL, generate=CFA.Model.NULL) 
Output.ALT.NULL <- sim(10, n=500, model=CFA.Model.NULL, generate=CFA.Model.ALT) 
Output.NULL.ALT <- sim(10, n=500, model=CFA.Model.ALT, generate=CFA.Model.NULL) 
Output.ALT.ALT <- sim(10, n=500, model=CFA.Model.ALT, generate=CFA.Model.ALT) 

# Plot the power based on the derived cutoff from the models analyzed on the null datasets
plotPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, nullNested=Output.NULL.NULL, 
	nullParent=Output.NULL.ALT)

# Plot the power by only CFI
plotPowerFitNested(Output.ALT.NULL, Output.ALT.ALT, nullNested=Output.NULL.NULL, 
	nullParent=Output.NULL.ALT, usedFit="CFI")

# The example of continous varying sample size. Note that more fine-grained 
# values of n is needed, e.g., n=seq(50, 500, 1)
Output.NULL.NULL2 <- sim(NULL, n=seq(50, 500, 25), model=CFA.Model.NULL, generate=CFA.Model.NULL) 
Output.ALT.NULL2 <- sim(NULL, n=seq(50, 500, 25), model=CFA.Model.NULL, generate=CFA.Model.ALT) 
Output.NULL.ALT2 <- sim(NULL, n=seq(50, 500, 25), model=CFA.Model.ALT, generate=CFA.Model.NULL) 
Output.ALT.ALT2 <- sim(NULL, n=seq(50, 500, 25), model=CFA.Model.ALT, generate=CFA.Model.ALT) 

# Plot logistic line for the power based on the derived cutoff from the null model 
# along sample size values
plotPowerFitNested(Output.ALT.NULL2, Output.ALT.ALT2, nullNested=Output.NULL.NULL2, 
	nullParent=Output.NULL.ALT2)

# Plot scatterplot for the power based on the derived cutoff from the null model 
# along sample size values
plotPowerFitNested(Output.ALT.NULL2, Output.ALT.ALT2, nullNested=Output.NULL.NULL2, 
	nullParent=Output.NULL.ALT2, logistic=FALSE)

# Plot scatterplot for the power based on the advanced CFI value
plotPowerFitNested(Output.ALT.NULL2, Output.ALT.ALT2, cutoff=c(CFI=-0.1), logistic=FALSE)
}
}
