\name{getCutoffNonNested}
\alias{getCutoffNonNested}
\title{
	Find fit indices cutoff for non-nested model comparison given a priori alpha level
}
\description{
	Extract fit indices information from the simulation of two models fitting on the datasets created from both models and getCutoff of fit indices given a priori alpha level
}
\usage{
getCutoffNonNested(dat1Mod1, dat1Mod2, dat2Mod1=NULL, dat2Mod2=NULL, 
alpha=.05, usedFit=NULL, onetailed=FALSE, nVal = NULL, pmMCARval = NULL, 
pmMARval = NULL, df = 0)
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
  \item{usedFit}{
	Vector of names of fit indices that researchers wish to get cutoffs from. The default is to get cutoffs of all fit indices.
}
  \item{onetailed}{
	If \code{TRUE}, the function will find the cutoff from one-tail test. If \code{FALSE}, the funciton will find the cutoff from two-tailed test.
}
  \item{nVal}{
	The sample size value that researchers wish to find the fit indices cutoffs from.
}
  \item{pmMCARval}{
	The percent missing completely at random value that researchers wish to find the fit indices cutoffs from.
}
  \item{pmMARval}{
	The percent missing at random value that researchers wish to find the fit indices cutoffs from.
}
  \item{df}{
	The degree of freedom used in spline method in predicting the fit indices by the predictors. If \code{df} is 0, the spline method will not be applied.
}
}
\value{
	One- or two-tailed cutoffs of several fit indices with a priori alpha level. The cutoff is based on the fit indices from Model 1 subtracted by the fit indices from Model 2.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\code{\linkS4class{SimResult}} for a detail of simResult
	\code{\link{getCutoff}} for a detail of finding cutoffs for absolute fit
	\code{\link{getCutoffNested}} for a detail of finding cutoffs for nested model comparison
	\code{\link{plotCutoffNonNested}} Plot cutoffs for non-nested model comparison
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

getCutoffNonNested(Output.A.A, Output.A.B, Output.B.A, Output.B.B)
getCutoffNonNested(Output.A.A, Output.A.B)
getCutoffNonNested(Output.B.B, Output.B.A)
}
