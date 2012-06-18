\name{plotCutoffNonNested}
\alias{plotCutoffNonNested}
\title{
	Plot sampling distributions of the differences in fit indices between non-nested models with fit indices cutoffs 
}
\description{
	This function will plot sampling distributions of the differences in fit indices between non-nested models. The users may add cutoffs by specifying the \code{alpha} level.
}
\usage{
plotCutoffNonNested(dat1Mod1, dat1Mod2, dat2Mod1=NULL, dat2Mod2=NULL, 
alpha=0.05, cutoff = NULL, usedFit = NULL, useContour = T, onetailed=FALSE)
}
\arguments{
  \item{dat1Mod1}{
	\code{\linkS4class{SimResult}} that saves the simulation of analyzing Model 1 by datasets created from Model 1
}
  \item{dat1Mod2}{
	\code{\linkS4class{SimResult}} that saves the simulation of analyzing Model 2 by datasets created from Model 1
}
  \item{dat2Mod1}{
	\code{\linkS4class{SimResult}} that saves the simulation of analyzing Model 1 by datasets created from Model 2
}
  \item{dat2Mod2}{
	\code{\linkS4class{SimResult}} that saves the simulation of analyzing Model 2 by datasets created from Model 2
}
  \item{alpha}{
	A priori alpha level
}
  \item{cutoff}{
	A priori cutoffs for fit indices, saved in a vector
}
  \item{usedFit}{
	Vector of names of fit indices that researchers wish to plot the sampling distribution. 
}
  \item{useContour}{
	If there are two of sample size, percent completely at random, and percent missing at random are varying, the \code{plotCutoff} function will provide 3D graph. Contour graph is a default. However, if this is specified as \code{FALSE}, perspective plot is used.
}
  \item{onetailed}{
	If \code{TRUE}, the function will find the cutoff from one-tail test. If \code{FALSE}, the funciton will find the cutoff from two-tailed test.
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
		\item \code{\link{getCutoffNonNested}} to find the difference in fit indices cutoffs for non-nested model comparison
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

plotCutoffNonNested(Output.A.A, Output.A.B, Output.B.A, Output.B.B)
plotCutoffNonNested(Output.A.A, Output.A.B)
plotCutoffNonNested(Output.A.A, Output.A.B, onetailed=TRUE)
}
