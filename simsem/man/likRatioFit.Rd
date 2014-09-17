\name{likRatioFit}
\alias{likRatioFit}
\title{
	Find the likelihood ratio (or Bayes factor) based on the bivariate distribution of fit indices
}
\description{
	Find the log-likelihood of the observed fit indices on Model 1 and 2 from the real data on the bivariate sampling distribution of fit indices fitting Model 1 and Model 2 by the datasets from the Model 1 and Model 2. Then, the likelihood ratio is computed (which may be interpreted as posterior odd). If the prior odd is 1 (by default), the likelihood ratio is equivalent to Bayes Factor.
}
\usage{
likRatioFit(outMod1, outMod2, dat1Mod1, dat1Mod2, dat2Mod1, dat2Mod2, 
usedFit=NULL, prior=1)
}
\arguments{
  \item{outMod1}{
	\code{\linkS4class{lavaan}} that saves the analysis result of the first model from the target dataset
}
  \item{outMod2}{
	\code{\linkS4class{lavaan}} that saves the analysis result of the second model from the target dataset
}
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
  \item{usedFit}{
	Vector of names of fit indices that researchers wish to getCutoffs from. The default is to getCutoffs of all fit indices.
}
  \item{prior}{
	The prior odds. The prior probability that Model 1 is correct over the prior probability that Model 2 is correct.
}
}
\value{
	The likelihood ratio (Bayes Factor) in preference of Model 1 to Model 2. If the value is greater than 1, Model 1 is preferred. If the value is less than 1, Model 2 is preferred.
}
\author{
    Sunthud Pornprasertmanit (Texas Tech University; \email{sunthud.pornprasertmanit@ttu.edu})
}
\seealso{
	\code{\linkS4class{SimResult}} for a detail of simResult
	\code{\link{pValueNested}} for a nested model comparison by the difference in fit indices
	\code{\link{pValueNonNested}} for a nonnested model comparison by the difference in fit indices
}
\examples{
\dontrun{
# Model A; Factor 1 --> Factor 2; Factor 2 --> Factor 3
library(lavaan)
loading <- matrix(0, 11, 3)
loading[1:3, 1] <- NA
loading[4:7, 2] <- NA
loading[8:11, 3] <- NA
path.A <- matrix(0, 3, 3)
path.A[2, 1] <- NA
path.A[3, 2] <- NA
model.A <- estmodel(LY=loading, BE=path.A, modelType="SEM", indLab=c(paste("x", 1:3, sep=""), 
	paste("y", 1:8, sep="")))

out.A <- analyze(model.A, PoliticalDemocracy)

# Model A; Factor 1 --> Factor 3; Factor 3 --> Factor 2
path.B <- matrix(0, 3, 3)
path.B[3, 1] <- NA
path.B[2, 3] <- NA
model.B <- estmodel(LY=loading, BE=path.B, modelType="SEM", indLab=c(paste("x", 1:3, sep=""), 
	paste("y", 1:8, sep="")))

out.B <- analyze(model.B, PoliticalDemocracy)

loading.mis <- matrix("runif(1, -0.2, 0.2)", 11, 3)
loading.mis[is.na(loading)] <- 0

# Create SimSem object for data generation and data analysis template
datamodel.A <- model.lavaan(out.A, std=TRUE, LY=loading.mis)
datamodel.B <- model.lavaan(out.B, std=TRUE, LY=loading.mis)

# Get sample size
n <- nrow(PoliticalDemocracy)

# The actual number of replications should be greater than 20.
output.A.A <- sim(20, n=n, model.A, generate=datamodel.A) 
output.A.B <- sim(20, n=n, model.B, generate=datamodel.A)
output.B.A <- sim(20, n=n, model.A, generate=datamodel.B)
output.B.B <- sim(20, n=n, model.B, generate=datamodel.B)

# Find the likelihood ratio ;The output may contain some warnings here. 
# When the number of replications increases (e.g., 1000), the warnings should disappear.
likRatioFit(out.A, out.B, output.A.A, output.A.B, output.B.A, output.B.B)
}
}
