\name{getCutoff}
\alias{getCutoff}
\alias{getCutoff-methods}
\alias{getCutoff,data.frame-method}
\alias{getCutoff,matrix-method}
\alias{getCutoff,SimResult-method}
\title{
	Find fit indices cutoff given a priori alpha level
}
\description{
	Extract fit indices information from the \code{\linkS4class{SimResult}} and getCutoff of fit indices given a priori alpha level
}
\usage{
getCutoff(object, alpha, revDirec = FALSE, usedFit = NULL, ...)
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
  \item{\dots}{
	Additional arguments.
  }
}
\section{Methods}{
	\describe{
	\item{signature(object="data.frame")}{
		This method will find the fit indices cutoff given a specified alpha level. The additional arguments are \code{predictor}, \code{predictorVal}, and \code{df}, which allows the fit indices predicted by any arbitrary independent variables (such as sample size or percent MCAR). The \code{predictor} is the \code{data.frame} of the predictor values. The number of rows of the \code{predictor} argument should be equal to the number of rows in the \code{object}. The \code{predictorVal} is the values of predictor that researchers would like to find the fit indices cutoffs from. The \code{df} is the degree of freedom used in spline method in predicting the fit indices by the predictors. If \code{df} is 0, the spline method will not be applied. 
	}
	\item{signature(object="matrix")}{
		The details are similar to the method for \code{data.frame}		
	}
	\item{signature(object="SimResult")}{
		This method will find the fit indices cutoff given a specified alpha level. The additional arguments are \code{nVal}, \code{pmMCARval}, \code{pmMARval}, and \code{df}, which are needed when using varying sample sizes or percent missing across replications in \code{\linkS4class{SimResult}}. The \code{nVal} is the sample size value that researchers wish to find the fit indices cutoffs from. The \code{pmMCARval} is the percent missing completely at random value that researchers wish to find the fit indices cutoffs from. The \code{pmMARval} is the percent missing at random value that researchers wish to find the fit indices cutoffs from. The \code{df} is the degree of freedom used in spline method in predicting the fit indices by the predictors. If \code{df} is 0, the spline method will not be applied.
	}
}}
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
