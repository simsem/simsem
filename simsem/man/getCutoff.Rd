\name{getCutoff}
\alias{getCutoff}
\title{
	Find fit indices cutoff given a priori alpha level
}
\description{
	Extract fit indices information from the \code{\linkS4class{SimResult}} and get the cutoffs of fit indices given a priori alpha level
}
\usage{
getCutoff(object, alpha, revDirec = FALSE, usedFit = NULL, nVal = NULL, 
	pmMCARval = NULL, pmMARval = NULL, df = 0)
}
\arguments{
  \item{object}{
	\code{\linkS4class{SimResult}} that saves the analysis results from multiple replications
}
  \item{alpha}{
	A priori alpha level
}
  \item{revDirec}{
	The default is to find criticl point on the side that indicates worse fit (the right side of RMSEA or the left side of CFI). If specifying as \code{TRUE}, the directions are reversed.
}
  \item{usedFit}{
	Vector of names of fit indices that researchers wish to getCutoffs from. The default is to getCutoffs of all fit indices.
  }
  \item{nVal}{
	The sample size value that researchers wish to find the fit indices cutoffs from. This argument is applicable for \code{\linkS4class{SimResult}} with varying sample sizes or percent missing across replications
  }
  \item{pmMCARval}{
	The percent missing completely at random value that researchers wish to find the fit indices cutoffs from. This argument is applicable for \code{\linkS4class{SimResult}} with varying sample sizes or percent missing across replications
  }
  \item{pmMARval}{
	The percent missing at random value that researchers wish to find the fit indices cutoffs from. This argument is applicable for \code{\linkS4class{SimResult}} with varying sample sizes or percent missing across replications
  }
  \item{df}{
	The degree of freedom used in spline method in predicting the fit indices by the predictors. If \code{df} is 0, the spline method will not be applied. This argument is applicable for \code{\linkS4class{SimResult}} with varying sample sizes or percent missing across replications
  }
}
\value{
	One-tailed cutoffs of several fit indices with a priori alpha level 
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
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
getCutoff(Output, 0.05)

# Finding the cutoff when the sample size is varied. Note that more fine-grained 
# values of n is needed, e.g., n=seq(50, 500, 1)
Output2 <- sim(NULL, model=CFA.Model, n=seq(50, 100, 10))

# Get the fit index cutoff when sample size is 75.
getCutoff(Output2, 0.05, nVal = 75)
}
}
