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
    Sunthud Pornprasertmanit (\email{psunthud@gmail.com})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimResult}} for simResult that used in this function.
		\item \code{\link{getCutoffNonNested}} to find the difference in fit indices cutoffs for non-nested model comparison
	}
}
\examples{
\dontrun{
# Model A: Factor 1 on Items 1-3 and Factor 2 on Items 4-8
loading.A <- matrix(0, 8, 2)
loading.A[1:3, 1] <- NA
loading.A[4:8, 2] <- NA
LY.A <- bind(loading.A, 0.7)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, "runif(1, 0.7, 0.9)")
RTE <- binds(diag(8))
CFA.Model.A <- model(LY = LY.A, RPS = RPS, RTE = RTE, modelType="CFA")

# Model B: Factor 1 on Items 1-4 and Factor 2 on Items 5-8
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

# Plot cutoffs for both model A and model B
plotCutoffNonNested(Output.A.A, Output.A.B, Output.B.A, Output.B.B)

# Plot cutoffs for the model A only
plotCutoffNonNested(Output.A.A, Output.A.B)

# Plot cutoffs for the model A with one-tailed test
plotCutoffNonNested(Output.A.A, Output.A.B, onetailed=TRUE)
}
}
