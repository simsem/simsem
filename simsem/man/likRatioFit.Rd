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
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\code{\linkS4class{SimResult}} for a detail of simResult
	\code{\link{pValueNested}} for a nested model comparison by the difference in fit indices
	\code{\link{pValueNonNested}} for a nonnested model comparison by the difference in fit indices
}
\examples{
\dontrun{
# It does not work now.

#library(lavaan)
#loading <- matrix(0, 11, 3)
#loading[1:3, 1] <- NA
#loading[4:7, 2] <- NA
#loading[8:11, 3] <- NA
#path.A <- matrix(0, 3, 3)
#path.A[2:3, 1] <- NA
#path.A[3, 2] <- NA
#param.A <- model(LY=bind(loading), BE=bind(path.A), modelType="SEM")

#model.A <- simModel(param.A, indLab=c(paste("x", 1:3, sep=""), paste("y", 1:8, sep="")))
#out.A <- run(model.A, PoliticalDemocracy)

#path.B <- matrix(0, 3, 3)
#path.B[1:2, 3] <- NA
#path.B[1, 2] <- NA
#param.B <- simParamSEM(LY=loading, BE=path.B)

#model.B <- simModel(param.B, indLab=c(paste("x", 1:3, sep=""), paste("y", 1:8, sep="")))
#out.B <- run(model.B, PoliticalDemocracy)

#u2 <- simUnif(-0.2, 0.2)
#loading.mis <- matrix(NA, 11, 3)
#loading.mis[is.na(loading)] <- 0
#LY.mis <- simMatrix(loading.mis, "u2")
#misspec <- simMisspecSEM(LY=LY.mis)

#output.A.A <- runFit(model.A, PoliticalDemocracy, 5, misspec=misspec)
#output.A.B <- runFit(model.A, PoliticalDemocracy, 5, misspec=misspec, analyzeModel=model.B)
#output.B.A <- runFit(model.B, PoliticalDemocracy, 5, misspec=misspec, analyzeModel=model.A)
#output.B.B <- runFit(model.B, PoliticalDemocracy, 5, misspec=misspec)

# The output may contain some warnings here. When the number of replications increases (e.g., 1000), the warnings should disappear.
#likRatioFit(out.A, out.B, output.A.A, output.A.B, output.B.A, output.B.B)
}
}
