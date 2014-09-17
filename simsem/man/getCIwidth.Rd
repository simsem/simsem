\name{getCIwidth}
\alias{getCIwidth}
\title{
	Find confidence interval width
}
\description{
	Find the median of confidence interval width or a confidence interval value given a degree of assurance (Lai & Kelley, 2011)
}
\usage{
getCIwidth(object, assurance = 0.50, nVal = NULL, pmMCARval = NULL, 
	pmMARval = NULL, df = 0)
}
\arguments{
  \item{object}{
	\code{\linkS4class{SimResult}} that saves the analysis results from multiple replications
}
  \item{assurance}{
	The percentile of the resulting confidence interval width. When assurance is 0.50, the median of the widths is provided. See Lai & Kelley (2011) for more details.
}
  \item{nVal}{
	The sample size value that researchers wish to find the confidence interval width from. This argument is applicable for \code{\linkS4class{SimResult}} with varying sample sizes or percent missing across replications
  }
  \item{pmMCARval}{
	The percent missing completely at random value that researchers wish to find the confidence interval width from. This argument is applicable for \code{\linkS4class{SimResult}} with varying sample sizes or percent missing across replications
  }
  \item{pmMARval}{
	The percent missing at random value that researchers wish to find the confidence interval width from. This argument is applicable for \code{\linkS4class{SimResult}} with varying sample sizes or percent missing across replications
  }
  \item{df}{
	The degree of freedom used in spline method in predicting the confidence interval width by the predictors. If \code{df} is 0, the spline method will not be applied. This argument is applicable for \code{\linkS4class{SimResult}} with varying sample sizes or percent missing across replications
  }
}
\value{
	The median of confidence interval width or a confidence interval given a degree of assurance
}
\references{
Lai, K., & Kelley, K. (2011). Accuracy in parameter estimation for targeted effects in structural equation modeling: Sample size planning for narrow confidence intervals. \emph{Psychological Methods, 16}, 127-148.
}
\author{
    Sunthud Pornprasertmanit (Texas Tech University; \email{sunthud.pornprasertmanit@ttu.edu})
}
\seealso{
	\code{\linkS4class{SimResult}} for a detail of simResult
}
\examples{
\dontrun{
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LY <- bind(loading, loadingValues)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
RTE <- binds(error.cor)
CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA")

# We make the examples running only 5 replications to save time.
# In reality, more replications are needed.
Output <- sim(5, n = 200, model=CFA.Model)

# Get the cutoff (critical value) when alpha is 0.05
getCIwidth(Output, assurance=0.80)

# Finding the cutoff when the sample size is varied. Note that more fine-grained 
# values of n is needed, e.g., n=seq(50, 500, 1)
Output2 <- sim(NULL, model=CFA.Model, n=seq(50, 100, 10))

# Get the fit index cutoff when sample size is 75.
getCIwidth(Output2, assurance=0.80, nVal = 75)
}
}
