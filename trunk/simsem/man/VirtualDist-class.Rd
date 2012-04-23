\name{VirtualDist-class}
\docType{class}
\alias{VirtualDist-class}
\alias{SimBeta-class}
\alias{SimBinom-class}
\alias{SimCauchy-class}
\alias{SimChisq-class}
\alias{SimExp-class}
\alias{SimF-class}
\alias{SimGamma-class}
\alias{SimGeom-class}
\alias{SimHyper-class}
\alias{SimLnorm-class}
\alias{SimLogis-class}
\alias{SimNbinom-class}
\alias{SimNorm-class}
\alias{SimPois-class}
\alias{SimT-class}
\alias{SimUnif-class}
\alias{SimWeibull-class}	
\alias{run,SimBeta-method}
\alias{run,SimBinom-method}
\alias{run,SimCauchy-method}
\alias{run,SimChisq-method}
\alias{run,SimExp-method}
\alias{run,SimF-method}
\alias{run,SimGamma-method}
\alias{run,SimGeom-method}
\alias{run,SimHyper-method}
\alias{run,SimLnorm-method}
\alias{run,SimLogis-method}
\alias{run,SimNbinom-method}
\alias{run,SimNorm-method}
\alias{run,SimPois-method}
\alias{run,SimT-method}
\alias{run,SimUnif-method}
\alias{run,SimWeibull-method}
\alias{summary,SimBeta-method}
\alias{summary,SimBinom-method}
\alias{summary,SimCauchy-method}
\alias{summary,SimChisq-method}
\alias{summary,SimExp-method}
\alias{summary,SimF-method}
\alias{summary,SimGamma-method}
\alias{summary,SimGeom-method}
\alias{summary,SimHyper-method}
\alias{summary,SimLnorm-method}
\alias{summary,SimLogis-method}
\alias{summary,SimNbinom-method}
\alias{summary,SimNorm-method}
\alias{summary,SimPois-method}
\alias{summary,SimT-method}
\alias{summary,SimUnif-method}
\alias{summary,SimWeibull-method}
\alias{summaryShort,SimBeta-method}
\alias{summaryShort,SimBinom-method}
\alias{summaryShort,SimCauchy-method}
\alias{summaryShort,SimChisq-method}
\alias{summaryShort,SimExp-method}
\alias{summaryShort,SimF-method}
\alias{summaryShort,SimGamma-method}
\alias{summaryShort,SimGeom-method}
\alias{summaryShort,SimHyper-method}
\alias{summaryShort,SimLnorm-method}
\alias{summaryShort,SimLogis-method}
\alias{summaryShort,SimNbinom-method}
\alias{summaryShort,SimNorm-method}
\alias{summaryShort,SimPois-method}
\alias{summaryShort,SimT-method}
\alias{summaryShort,SimUnif-method}
\alias{summaryShort,SimWeibull-method}	
\alias{plotDist,VirtualDist-method}
\alias{skew,VirtualDist-method}
\alias{kurtosis,VirtualDist-method}
\title{Distribution Objects}
\description{
List of all distribution objects. These distribution objects can be used to specify random parameters or specify a marginal distribution of a variable.
}
\section{Distributions}{
	Here is the list of all distribution objects and the link to their constructors.
	\itemize{
		\item \code{\link{simBeta}} Beta Distribution
		\item \code{\link{simBinom}} Binomial Distribution
		\item \code{\link{simCauchy}} Cauchy Distribution
		\item \code{\link{simChisq}} Chi-squared Distribution
		\item \code{\link{simExp}} Exponential Distribution
		\item \code{\link{simF}} F Distribution
		\item \code{\link{simGamma}} Gamma Distribution
		\item \code{\link{simGeom}} Geometric Distribution
		\item \code{\link{simHyper}} Hypergeometric Distribution
		\item \code{\link{simLnorm}} Log Normal Distribution
		\item \code{\link{simLogis}} Logistic Distribution
		\item \code{\link{simNbinom}} Negative Binomial Distribution
		\item \code{\link{simNorm}} Normal Distribution
		\item \code{\link{simPois}} Poisson Distribution
		\item \code{\link{simT}} t Distribution
		\item \code{\link{simUnif}} Uniform Distribution
		\item \code{\link{simWeibull}} Weibull Distribution
	}
}
\section{Methods}{
  \describe{
    \item{\code{\link{run}}}{Create a random number from the specified distribution. }
    \item{\code{summary}}{Summarize information withinin the object. }
    \item{\code{\link{summaryShort}}}{Summarize information within the object in a short form. }
	\item{\code{\link{plotDist}}}{Plot a distribution of the distribution object. Arguments: \code{xlim} is the range of plotting values (should be a vector of two values: lower and upper bound), \code{reverse} is to mirror the distribution, such as changing chi-square distribution from positively skew to negatively skew. }
	\item{\code{\link{skew}}}{Find a skewness of a distribution.}
	\item{\code{\link{kurtosis}}}{Find an excessive kurtosis of a distribution.}
	 }
}
\details{
	These distribution objects can be used to specify random parameters or marginal distribution of variables in Gaussian copula. The random parameter feature is to make data generation parameters different across replications in a simualtion study. The distribution object can be specified as random parameters in \code{\link{simMatrix}}, \code{\link{symMatrix}}, \code{\link{simVector}}, and \code{\link{simResult}} (in \code{n}, \code{pmMCAR}, and \code{pmMAR}). The distribution object can also be used for specifying marginal distribution of factors, measurement errors, or indicators. See the data distribution object, \code{\link{simDataDist}}, for how to model marginal distribution of variables, which will be put in setting the data object up, \code{\link{simData}}.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	List of all distribution objects.
	\itemize{
		\item \code{\linkS4class{SimBeta}} Beta Distribution
		\item \code{\linkS4class{SimBinom}} Binomial Distribution
		\item \code{\linkS4class{SimCauchy}} Cauchy Distribution
		\item \code{\linkS4class{SimChisq}} Chi-squared Distribution
		\item \code{\linkS4class{SimExp}} Exponential Distribution
		\item \code{\linkS4class{SimF}} F Distribution
		\item \code{\linkS4class{SimGamma}} Gamma Distribution
		\item \code{\linkS4class{SimGeom}} Geometric Distribution
		\item \code{\linkS4class{SimHyper}} Hypergeometric Distribution
		\item \code{\linkS4class{SimLnorm}} Log Normal Distribution
		\item \code{\linkS4class{SimLogis}} Logistic Distribution
		\item \code{\linkS4class{SimNbinom}} Negative Binomial Distribution
		\item \code{\linkS4class{SimNorm}} Normal Distribution
		\item \code{\linkS4class{SimPois}} Poisson Distribution
		\item \code{\linkS4class{SimT}} t Distribution
		\item \code{\linkS4class{SimUnif}} Uniform Distribution
		\item \code{\linkS4class{SimWeibull}} Weibull Distribution
	}
	Here are the list of possible applications of a distribution object
	\itemize{
		\item \code{\link{simMatrix}} Random parameter matrix. A distribution object can be used to create random parameter.
		\item \code{\link{symMatrix}} Random parameter symmetric matrix. A distribution object can be used to create random parameter.
		\item \code{\link{simVector}} Random parameter vector. A distribution object can be used to create random parameter.
		\item \code{\link{simResult}} Result object that saves the result of a simulation study. A distribution object can be used to vary sample size (\code{n}), proportion completely missing at random (\code{pmMCAR}), or proportion missing at random (\code{pmMAR}), which make those factors (e.g., sample size) different across replications.
		\item \code{\link{simDataDist}} Data distribution object. A distribution object can be used to specify marginal distributions of variables (which can be factors, measurement errors, or indicators).
	}
}
\examples{
showClass("VirtualDist")
u1 <- simUnif(0, 1)
chi3 <- simChisq(3)
summary(chi3)
skew(chi3)
kurtosis(chi3)
plotDist(chi3)
plotDist(chi3, reverse=TRUE)
}
\keyword{classes}
