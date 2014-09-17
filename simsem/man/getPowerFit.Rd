\name{getPowerFit}
\alias{getPowerFit}
\title{
	Find power in rejecting alternative models based on fit indices criteria
}
\description{
	Find the proportion of fit indices that indicate worse fit than a specified cutoffs. The cutoffs may be calculated from \code{\link{getCutoff}} of the null model.
}
\usage{
getPowerFit(altObject, cutoff = NULL, nullObject = NULL, revDirec = FALSE, 
usedFit = NULL, alpha = 0.05, nVal = NULL, pmMCARval = NULL, pmMARval = NULL, 
condCutoff = TRUE, df = 0)
}
\arguments{
  \item{altObject}{
	\code{\linkS4class{SimResult}} that indicates alternative model that users wish to reject
}
  \item{cutoff}{
	Fit indices cutoffs from null model or users. This should be a vector with a specified fit indices names as the name of vector elements. The \code{cutoff} cannot be specified if the \code{nullObject} is specified.
}
  \item{nullObject}{
	The \code{\linkS4class{SimResult}} that contains the simulation result from fitting the null model by the data from the null model. The \code{nullObject} cannot be specified if the \code{cutoff} is specified.
}
  \item{revDirec}{
	Reverse the direction of deciding a power by fit indices (e.g., less than --> greater than). The default is to count the proportion of fit indices that indicates lower fit to the model, such as how many RMSEA in the alternative model that is worse than cutoffs. The direction can be reversed by setting as \code{TRUE}.
}
  \item{usedFit}{
	The vector of names of fit indices that researchers wish to get powers from. The default is to get powers of all fit indices
}
  \item{alpha}{
	The alpha level used to find the cutoff if the \code{nullObject} is specified. This argument is not applicable if the \code{cutoff} is specified.
}
  \item{nVal}{
	The sample size value that researchers wish to find the power from. This argument is applicable when \code{altObject} has a random sample size. 
}
  \item{pmMCARval}{
	The percent missing completely at random value that researchers wish to find the power from. This argument is applicable when \code{altObject} has a random percent missing completely at random. 
}
  \item{pmMARval}{
	The percent missing at random value that researchers wish to find the power from. This argument is applicable when \code{altObject} has a random percent missing at random. 
}
  \item{condCutoff}{
	A logical value to use a conditional quantile method (if \code{TRUE}) or logistic regression method (if \code{FALSE}) to find the power. The conditional quantile method use quantile regression to find the quantile of the cutoff on the alternative sampling distribution that varies \code{nVal}, \code{pmMCARval}, or \code{pmMARval}. The value of 1 - quantile will be reported as the power given the set of \code{nVal}, \code{pmMCARval}, and \code{pmMARval}. The logistic regression method is based on transforming the fit indices value to reject/retain decision first. Then, the logistic regression is used to predict reject/retain decision given the set of \code{nVal}, \code{pmMCARval}, and \code{pmMARval}. The predicted probability is reported as a power. This argument is applicable for specification of \code{cutoff} only.
}
  \item{df}{
	The degree of freedom used in spline method in quantile regression (\code{condCutoff = TRUE}). If \code{df} is 0, which is the default, the spline method will not be applied.
}
}
\value{
	List of power given different fit indices. The \code{TraditionalChi} means the proportion of replications that are rejected by the traditional chi-square test.
}
\author{
    Sunthud Pornprasertmanit (Texas Tech University; \email{sunthud.pornprasertmanit@ttu.edu})
}
\seealso{
	\itemize{
		\item \code{\link{getCutoff}} to find the cutoffs from null model.
		\item \code{\linkS4class{SimResult}} to see how to create simResult
	}
}
\examples{
\dontrun{
# Null model with one factor
loading.null <- matrix(0, 6, 1)
loading.null[1:6, 1] <- NA
LY.NULL <- bind(loading.null, 0.7)
RPS.NULL <- binds(diag(1))
RTE <- binds(diag(6))
CFA.Model.NULL <- model(LY = LY.NULL, RPS = RPS.NULL, RTE = RTE, modelType="CFA")

# We make the examples running only 5 replications to save time.
# In reality, more replications are needed.
Output.NULL <- sim(5, n=500, model=CFA.Model.NULL)

# Get the fit index cutoff from the null model
Cut.NULL <- getCutoff(Output.NULL, 0.05)

# Alternative model with two factor
loading.alt <- matrix(0, 6, 2)
loading.alt[1:3, 1] <- NA
loading.alt[4:6, 2] <- NA
LY.ALT <- bind(loading.alt, 0.7)
latent.cor.alt <- matrix(NA, 2, 2)
diag(latent.cor.alt) <- 1
RPS.ALT <- binds(latent.cor.alt, "runif(1, 0.7, 0.9)")
CFA.Model.ALT <- model(LY = LY.ALT, RPS = RPS.ALT, RTE = RTE, modelType="CFA")

# We make the examples running only 5 replications to save time.
# In reality, more replications are needed.
Output.ALT <- sim(5, n=500, model=CFA.Model.NULL, generate=CFA.Model.ALT)

# Get the power based on the derived cutoff
getPowerFit(Output.ALT, cutoff=Cut.NULL)

# Get the power based on the rule of thumb proposed by Hu & Bentler (1999)
Rule.of.thumb <- c(RMSEA=0.05, CFI=0.95, TLI=0.95, SRMR=0.06)
getPowerFit(Output.ALT, cutoff=Rule.of.thumb, usedFit=c("RMSEA", "CFI", "TLI", "SRMR"))

# The example of continous varying sample size. Note that more fine-grained 
# values of n is needed, e.g., n=seq(50, 500, 1)
Output.NULL2 <- sim(NULL, n=seq(50, 500, 50), model=CFA.Model.NULL, generate=CFA.Model.NULL)
Output.ALT2 <- sim(NULL, n=seq(50, 500, 50), model=CFA.Model.NULL, generate=CFA.Model.ALT)

# Get the power based on the derived cutoff from the null model at the sample size of 250
getPowerFit(Output.ALT2, nullObject=Output.NULL2, nVal=250)
}
}
