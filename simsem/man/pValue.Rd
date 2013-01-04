\name{pValue}
\alias{pValue}
\alias{pValue-methods}
\alias{pValue,ANY-method}
\alias{pValue,numeric,vector-method}
\alias{pValue,numeric,data.frame-method}
\alias{pValue,lavaan,SimResult-method}
\title{
	Find p-values (1 - percentile)
}
\description{
	This function will provide \emph{p} value from comparing number and vector or the analytic result to the observed data (in \code{\linkS4class{lavaan}}) and the simulation result (in \code{\link{SimResult}}).
}
\usage{
pValue(target, dist, ...)
}
\arguments{
  \item{target}{
	A value, multiple values, or a lavaan object used to find p values. This argument could be a cutoff of a fit index.
}
  \item{dist}{
	The comparison distribution, which can be a vector of numbers, a data frame, or a result object.
}
  \item{\dots}{
	Other values that will be explained specifically for each class
}
}
\section{Methods}{
	\describe{
	\item{signature(target="numeric", dist="vector")}{
		This method will find the \emph{p} value (quantile rank) of the \code{target} value on the \code{dist} vector. The additional arguments are \code{revDirec}, \code{x} , \code{xval}, \code{condCutoff}, and \code{df}. The \code{revDirec} is a logical argument whether to reverse the direction of comparison. If \code{TRUE}, the proportion of the \code{dist} that is lower than \code{target} value is reported. If \code{FALSE}, the proportion of the \code{dist} that is higher than the \code{target} value is reported. The \code{x} is the \code{data.frame} of the predictor values. The number of rows of the \code{x} argument should be equal to the number of rows in the \code{dist}. The \code{xval} is the values of predictor that researchers would like to find the fit indices cutoffs from. The \code{condCutoff} is a logical argument. If \code{TRUE}, the cutoff is applicable only a given value of \code{xval}. If \code{FALSE}, the cutoff is applicable in any values of \code{predictor}. The \code{df} is the degree of freedom used in spline method in predicting the fit indices by the predictors. If \code{df} is 0, the spline method will not be applied. 
	}
	\item{signature(target="numeric", dist="data.frame")}{
		This method will find the \emph{p} value of each columns in the \code{dist} based on the value specified in the \code{target}. The additional arguments are \code{revDirec}, \code{x} , \code{xval}, \code{df}, and \code{asLogical}. The \code{revDirec} is a logical vector whether to reverse the direction of comparison. If \code{TRUE}, the proportion of the \code{dist} that is lower than \code{target} value is reported. If \code{FALSE}, the proportion of the \code{dist} that is higher than the \code{target} value is reported. The \code{x} is the \code{data.frame} of the predictor values. The number of rows of the \code{x} argument should be equal to the number of rows in the \code{dist}. The \code{xval} is the values of predictor that researchers would like to find the fit indices cutoffs from. The \code{df} is the degree of freedom used in spline method in predicting the fit indices by the predictors. If \code{df} is 0, the spline method will not be applied. The \code{asLogical} is to provide the result as the matrix of significance result (\code{TRUE}) or just the proportion of significance result (\code{FALSE}).
	}
	\item{signature(target="lavaan", dist="SimResult")}{
		This method will find the \emph{p} value of the analysis result compared to the simulated sampling distribution in a result object (\code{SimResult}). The additional arguments are \code{usedFit}, \code{nVal}, \code{pmMCARval}, \code{pmMARval}, and \code{df}. The \code{usedFit} is the vector of names of fit indices that researchers wish to find the \emph{p} value from. The \code{nVal} is the sample size value that researchers wish to find the fit indices cutoffs from. The \code{pmMCARval} is the percent missing completely at random value that researchers wish to find the fit indices cutoffs from. The \code{pmMARval} is the percent missing at random value that researchers wish to find the fit indices cutoffs from. The \code{df} is the degree of freedom used in spline method in predicting the fit indices by the predictors. If \code{df} is 0, the spline method will not be applied.
	}
}}
\value{
	Mostly, this function provides a vector of \emph{p} values based on the comparison. If the target is a model output object and dist is a result object, the \emph{p} values of fit indices are provided, as well as two additional values: \code{andRule} and \code{orRule}. The \code{andRule} is based on the principle that the model is retained only when all fit indices provide good fit. The proportion is calculated from the number of replications that have all fit indices indicating a better model than the observed data. The proportion from the \code{andRule} is the most stringent rule in retaining a hypothesized model. The \code{orRule} is based on the principle that the model is retained only when at least one fit index provides good fit. The proportion is calculated from the number of replications that have at least one fit index indicating a better model than the observed data. The proportion from the \code{orRule} is the most lenient rule in retaining a hypothesized model.
}
\details{
	In comparing fit indices, the \emph{p} value is the proportion of the number of replications that provide poorer fit (e.g., less CFI value or greater RMSEA value) than the analysis result from the observed data. If the \code{target} is a critical value (e.g., fit index cutoff) and the \code{dist} is the sampling distribution underlying the alternative hypothesis, this function can provide a statistical power.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\link{SimResult}} to run a simulation study
	}
}
\examples{
\dontrun{
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
targetmodel <- estmodel(LY=loading, modelType="CFA", indLab=paste("x", 1:9, sep=""))
out <- analyze(targetmodel, HolzingerSwineford1939)

loading.trivial <- matrix("runif(1, -0.2, 0.2)", 9, 3)
loading.trivial[is.na(loading)] <- 0
mismodel <- model.lavaan(out, std=TRUE, LY=loading.trivial)

# The actual number of replications should be much greater than 20.
simout <- sim(20, n=nrow(HolzingerSwineford1939), mismodel)

# Find the p-value comparing the observed fit indices against the simulated 
# sampling distribution of fit indices
pValue(out, simout)
}
}
