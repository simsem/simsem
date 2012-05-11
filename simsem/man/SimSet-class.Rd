\name{SimSet-class}
\Rdversion{1.1}
\docType{class}
\alias{SimSet-class}
\alias{run,SimSet-method}
\alias{summary,SimSet-method}
\alias{extract,SimSet-method}

\title{Class \code{"SimSet"}}
\description{
	Set of vectors and matrices that saves free parameters and parameter values (CFA, Path analysis, or SEM)
}
\section{Objects from the Class}{
Object can be created by \code{\link{simSetCFA}}, \code{\link{simSetPath}}, or \code{\link{simSetSEM}}, for CFA, Path analysis, or SEM model, respectively. Objects can be also created by calls of the form \code{new("SimSet", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{modelType}:}{Model type (CFA, Path, or SEM) }
    \item{\code{LY}:}{Factor loading matrix between endogenous factors and Y indicators }
    \item{\code{TE}:}{Covariance matrix between Y measurement error }
    \item{\code{RTE}:}{Correlation matrix between Y measurement error }
    \item{\code{VTE}:}{Variance of Y measurement error }
    \item{\code{PS}:}{Residual covariance of endogenous factors  }
    \item{\code{RPS}:}{Residual correlation of endogenous factors  }
    \item{\code{VPS}:}{Residual variances of endogenous factors }
    \item{\code{BE}:}{Regression effect among endogenous factors }
    \item{\code{TY}:}{Measurement intercepts of Y indicators }
    \item{\code{AL}:}{Factor intercepts of endogenous factors }
    \item{\code{ME}:}{Factor means of endogenous factors }
    \item{\code{MY}:}{Total Mean of Y indicators }
    \item{\code{VE}:}{Total variance of endogenous factors }
    \item{\code{VY}:}{Total variance of Y indicators }
    \item{\code{LX}:}{Factor loading matrix between exogenous factors and X indicators }
    \item{\code{TD}:}{Covariance matrix between X measurement error }
    \item{\code{RTD}:}{Correlation matrix between X measurement error }
    \item{\code{VTD}:}{Variance of X measurement error }
    \item{\code{PH}:}{Covariance among exogenous factors }
    \item{\code{RPH}:}{Correlation among exogenous factors }
    \item{\code{GA}:}{Regression effect from exogenous factors to endogenous factors }
    \item{\code{TX}:}{Measurement intercepts of X indicators }
    \item{\code{KA}:}{Factor Mean of exogenous factors }
    \item{\code{MX}:}{Total Mean of X indicators }
    \item{\code{VPH}:}{Variance of exogenous factors }
    \item{\code{VX}:}{Total variance of X indicators }
    \item{\code{TH}:}{Measurement error covariance between X indicators and Y indicators }
    \item{\code{RTH}:}{Measurement error correlation between X indicators and Y indicators }
  }
}
\section{Methods}{
  \describe{
    \item{run}{Create a sample of parameters in this object. In other words, draw a sample from all random parameters which is represented in \code{\linkS4class{VirtualDist}}. }
    \item{summary}{Get the summary of model specification }
    \item{extract}{Extract elements from a simSet. There are several additional arguments. First, if \code{yOnly} is \code{TRUE}, then the result will provide only Y side. Second, \code{y} is the index of indicators in Y side to be extracted. Third, \code{e} is the index of factors in Y side to be extracted. Fourth, \code{x} is the index of the indicators in X side to be extracted. Finally, \code{k} is the index of the factors in X side to be extracted. }
	 }
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item Create an object this class by CFA, Path Analysis, or SEM model by \code{\link{simSetCFA}}, \code{\link{simSetPath}}, or \code{\link{simSetSEM}}, respectively.
		\item See how to specify model misspecification by \code{\linkS4class{SimMisspec}}.
	}
}
\examples{
showClass("SimSet")

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- simMatrix(loading, loadingValues)
summary(LX)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, 0.5)

# Error Correlation Object
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
RTD <- symMatrix(error.cor)

CFA.Model <- simSetCFA(LX = LX, RPH = RPH, RTD = RTD)
summary(CFA.Model)
#run(CFA.Model)

CFA.Model2 <- extract(CFA.Model, y=1:3, e=1)
summary(CFA.Model2)
}
