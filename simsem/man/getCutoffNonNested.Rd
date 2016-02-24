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
    Sunthud Pornprasertmanit (\email{psunthud@gmail.com})
}
\seealso{
	\code{\linkS4class{SimResult}} for a detail of simResult
	\code{\link{getCutoff}} for a detail of finding cutoffs for absolute fit
	\code{\link{getCutoffNested}} for a detail of finding cutoffs for nested model comparison
	\code{\link{plotCutoffNonNested}} Plot cutoffs for non-nested model comparison
}
\examples{
\dontrun{
# Model A: Factor 1 with items 1-3 and Factor 2 with items 4-8
loading.A <- matrix(0, 8, 2)
loading.A[1:3, 1] <- NA
loading.A[4:8, 2] <- NA
LY.A <- bind(loading.A, 0.7)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, "runif(1, 0.7, 0.9)")
RTE <- binds(diag(8))
CFA.Model.A <- model(LY = LY.A, RPS = RPS, RTE = RTE, modelType="CFA")

# Model B: Factor 1 with items 1-4 and Factor 2 with items 5-8
loading.B <- matrix(0, 8, 2)
loading.B[1:4, 1] <- NA
loading.B[5:8, 2] <- NA
LY.B <- bind(loading.B, 0.7)
CFA.Model.B <- model(LY = LY.B, RPS = RPS, RTE = RTE, modelType="CFA")

# The actual number of replications should be greater than 10.
Output.A.A <- sim(10, n=500, model=CFA.Model.A, generate=CFA.Model.A)
Output.A.B <- sim(10, n=500, model=CFA.Model.B, generate=CFA.Model.A)
Output.B.A <- sim(10, n=500, model=CFA.Model.A, generate=CFA.Model.B)
Output.B.B <- sim(10, n=500, model=CFA.Model.B, generate=CFA.Model.B)

# Find the cutoffs from the sampling distribution to reject model A (model 1)
# and to reject model B (model 2)
getCutoffNonNested(Output.A.A, Output.A.B, Output.B.A, Output.B.B)

# Find the cutoffs from the sampling distribution to reject model A (model 1)
getCutoffNonNested(Output.A.A, Output.A.B)

# Find the cutoffs from the sampling distribution to reject model B (model 1)
getCutoffNonNested(Output.B.B, Output.B.A)
}
}
