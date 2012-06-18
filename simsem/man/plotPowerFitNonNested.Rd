\name{plotPowerFitNonNested}
\alias{plotPowerFitNonNested}
\title{
	Plot power of rejecting a non-nested model based on a difference in fit index 
}
\description{
	Plot the proportion of the difference in fit indices from one model that does not in the range of sampling distribution from another model (reject that the dataset comes from the second model) or indicates worse fit than a specified cutoff. This plot can show the proportion in the second model that does not in the range of sampling distribution from the first model too.
}
\usage{
plotPowerFitNonNested(dat2Mod1, dat2Mod2, dat1Mod1=NULL, dat1Mod2=NULL, 
cutoff = NULL, usedFit = NULL, alpha = 0.05, contN = TRUE, contMCAR = TRUE, 
contMAR = TRUE, useContour = TRUE, logistic = TRUE, onetailed = FALSE)
}
\arguments{
  \item{dat2Mod1}{
	\code{\linkS4class{SimResult}} that saves the simulation of analyzing Model 1 by datasets created from Model 2
}
  \item{dat2Mod2}{
	\code{\linkS4class{SimResult}} that saves the simulation of analyzing Model 2 by datasets created from Model 2
}
  \item{dat1Mod1}{
	\code{\linkS4class{SimResult}} that saves the simulation of analyzing Model 1 by datasets created from Model 1
}
  \item{dat1Mod2}{
	\code{\linkS4class{SimResult}} that saves the simulation of analyzing Model 2 by datasets created from Model 1
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
  \item{onetailed}{
	If \code{TRUE}, the function will use the cutoff from one-tail test. If \code{FALSE}, the funciton will use the cutoff from two-tailed test.
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
		\item \code{\link{getCutoffNonNested}} to find the cutoffs of the differences in fit indices for non-nested model comparison
		\item \code{\link{plotCutoffNonNested}} to visualize the cutoffs of the differences in fit indices for non-nested model comparison
		\item \code{\link{getPowerFitNonNested}} to find the power in rejecting the non-nested model by the difference in fit indices cutoffs
	}
}
\examples{
n1 <- simNorm(0, 0.1)
u79 <- simUnif(0.7, 0.9)

loading.A <- matrix(0, 8, 2)
loading.A[1:3, 1] <- NA
loading.A[4:8, 2] <- NA
LX.A <- simMatrix(loading.A, 0.7)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, "u79")
RTD <- symMatrix(diag(8))
CFA.Model.A <- simSetCFA(LY = LX.A, RPS = RPH, RTE = RTD)

error.cor.mis <- matrix(NA, 8, 8)
diag(error.cor.mis) <- 1
RTD.Mis <- symMatrix(error.cor.mis, "n1")
CFA.Model.A.Mis <- simMisspecCFA(RTE = RTD.Mis)

loading.B <- matrix(0, 8, 2)
loading.B[1:4, 1] <- NA
loading.B[5:8, 2] <- NA
LX.B <- simMatrix(loading.B, 0.7)
CFA.Model.B <- simSetCFA(LY = LX.B, RPS = RPH, RTE = RTD)

SimData.A <- simData(CFA.Model.A, 500)
SimData.B <- simData(CFA.Model.B, 500)

SimModel.A <- simModel(CFA.Model.A)
SimModel.B <- simModel(CFA.Model.B)

# The actual number of replications should be greater than 10.
Output.A.A <- simResult(10, SimData.A, SimModel.A)
Output.A.B <- simResult(10, SimData.A, SimModel.B)
Output.B.A <- simResult(10, SimData.B, SimModel.A)
Output.B.B <- simResult(10, SimData.B, SimModel.B)

plotPowerFitNonNested(Output.B.A, Output.B.B, dat1Mod1=Output.A.A, dat1Mod2=Output.A.B)
plotPowerFitNonNested(Output.B.A, Output.B.B, cutoff=c(AIC=0, BIC=0))
}
