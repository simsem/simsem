\name{pValue}
\alias{pValue}
\alias{pValue-methods}
\alias{pValue,ANY-method}
\alias{pValue,numeric,vector-method}
\alias{pValue,numeric,data.frame-method}
\alias{pValue,SimModelOut,SimResult-method}
\title{
	Find p-values (1 - percentile)
}
\description{
	This function will provide p value from comparing number and vector or the analysis result of the observed data (in \code{\linkS4class{SimModelOut}}) and the simulation result (in \code{\linkS4class{SimResult}}).
}
\usage{
pValue(target, dist, ...)
}
\arguments{
  \item{target}{
	A value, multiple values, or a model output object used to find p values.
}
  \item{dist}{
	The comparison distribution, which can be a vector of numbers, a data frame, or a result object.
}
  \item{\dots}{
	Other values that will be explained specifically for each class
}
}
\value{
	p values based on the comparison. If the target is a model output object and dist is a result object, the p values of fit indices are provided, as well as two additional values: \code{andRule} and \code{orRule}. The \code{andRule} provides the probability of the number of replications that all fit indices in the replication are poorer fit than the model output object. The \code{orRule} provides the probability of the number of replications that at least one fit index in the replication is poorer than the model output object. 
}
\section{Details in \dots}{
	\itemize{
		\item \emph{revDirec:} Desired sample size
		\item \emph{asLogical:} Model \emph{misspecification} matrices that are created by \code{\link{simMisspecCFA}}, \code{\link{simMisspecPath}}, or \code{\link{simMisspecSEM}}.
		\item \emph{usedFit:} Equality constraints that are created by \code{\link{simEqualCon}}. This will specify equality econstraints of parameters in data generation process.
	}
}
\details{
	In comparing fit indices, the p value is the proportion of the number of replications that provide poorer fit (e.g., less CFI value or greater RMSEA value) than the analysis result from the observed data.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimModelOut}} to see how to get the analysis result of observed data
		\item \code{\linkS4class{SimResult}} to run a simulation study
		\item \code{\link{runFit}} to run a simulation study based on the parameter estimates from the analysis result of observed data
	}
}
\examples{
# Compare number with a vector
pValue(0.5, rnorm(1000, 0, 1))

# Compare numbers with a data frame
pValue(c(0.5, 0.2), data.frame(rnorm(1000, 0, 1), runif(1000, 0, 1)))

# Compare an analysis result with a result of simulation study
library(lavaan)
loading <- matrix(0, 9, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
model <- simParamCFA(LY=loading)
SimModel <- simModel(model, indLab=paste("x", 1:9, sep=""))
u2 <- simUnif(-0.2, 0.2)
loading.trivial <- matrix(NA, 9, 3)
loading.trivial[is.na(loading)] <- 0
LY.trivial <- simMatrix(loading.trivial, "u2")
mis <- simMisspecCFA(LY = LY.trivial)
out <- run(SimModel, HolzingerSwineford1939)
Output2 <- runFit(out, HolzingerSwineford1939, 20, mis)
pValue(out, Output2)
}
