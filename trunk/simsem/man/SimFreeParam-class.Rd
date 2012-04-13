\name{SimFreeParam-class}
\Rdversion{1.1}
\docType{class}
\alias{SimFreeParam-class}
\alias{summary,SimFreeParam-method}

\title{Class \code{"SimFreeParam"}}
\description{
	Set of vectors and matrices that saves free parameters (CFA, Path analysis, or SEM)
}
\section{Objects from the Class}{
Object can be created by \code{\link{simParamCFA}}, \code{\link{simParamPath}}, or \code{\link{simParamSEM}}, for CFA, Path analysis, or SEM model, respectively. Objects can be also created by calls of the form \code{new("SimFreeParam", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{modelType}:}{Model type (CFA, Path, or SEM) }
    \item{\code{LY}:}{Factor loading matrix between endogenous factors and Y indicators }
    \item{\code{TE}:}{Covariance matrix between Y measurement error }
    \item{\code{PS}:}{Residual covariance of endogenous factors  }
    \item{\code{BE}:}{Regression effect among endogenous factors }
    \item{\code{TY}:}{Measurement intercepts of Y indicators }
    \item{\code{AL}:}{Factor intercepts of endogenous factors }
    \item{\code{LX}:}{Factor loading matrix between exogenous factors and X indicators }
    \item{\code{TD}:}{Covariance matrix between X measurement error }
    \item{\code{PH}:}{Covariance among exogenous factors }
    \item{\code{GA}:}{Regreeion effect from exogenous factors to endogenous factors }
    \item{\code{TX}:}{Measurement intercepts of X indicators }
    \item{\code{KA}:}{Factor Mean of exogenous factors }
    \item{\code{TH}:}{Measurement error covariance between X indicators and Y indicators }
  }
}
\section{Methods}{
  \describe{
    \item{summary}{Get the summary of model specification. }
    \item{extract}{Extract elements from a simSet. There are several additional arguments. First, if \code{yOnly} is \code{TRUE}, then the result will provide only Y side. Second, \code{y} is the index of indicators in Y side to be extracted. Third, \code{e} is the index of factors in Y side to be extracted. Fourth, \code{x} is the index of the indicators in X side to be extracted. Finally, \code{k} is the index of the factors in X side to be extracted. }
	 }
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item Create an object this class by CFA, Path Analysis, or SEM model by \code{\link{simParamCFA}}, \code{\link{simParamPath}}, or \code{\link{simParamSEM}}, respectively.
		\item See how to use this object for data analysis by \code{\link{simModel}}.
	}
}
\examples{
showClass("SimFreeParam")

library(lavaan)
loading <- matrix(0, 9, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
HS.Model <- simParamCFA(LX = loading)
summary(HS.Model)
SimModel <- simModel(HS.Model, indLab=paste("x", 1:9, sep=""))
out <- run(SimModel, HolzingerSwineford1939)
summary(out)

HS.Model2 <- extract(HS.Model, y=1:3)
summary(HS.Model2)
}
