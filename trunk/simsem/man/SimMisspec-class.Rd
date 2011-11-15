\name{SimMisspec-class}
\Rdversion{1.1}
\docType{class}
\alias{SimMisspec-class}
\alias{run,SimMisspec-method}

\title{Class \code{"SimMisspec"}}
\description{
Misspecification model added on true model specifiction. This class contains \code{\linkS4class{SimVector}}, \code{\linkS4class{SimMatrix}}, and \code{\linkS4class{SymMatrix}} specifying misspecification.
}
\section{Objects from the Class}{
Object can be created by \code{\link{simMisspecCFA}}, \code{\link{simMisspecPath}}, or \code{\link{simMisspecSEM}}, for CFA, Path analysis, or SEM model, respectively. Objects can be also created by calls of the form \code{new("SimMisspec", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{modelType}:}{Model type (CFA, Path, or SEM) }
    \item{\code{LY}:}{Factor loading matrix between endogenous factors and Y indicators }
    \item{\code{TE}:}{Correlation matrix between Y measurement error }
    \item{\code{VTE}:}{Variance of Y measurement error }
    \item{\code{PS}:}{Residual correlation of endogenous factors  }
    \item{\code{VPS}:}{Residual variances of endogenous factors }
    \item{\code{BE}:}{Regression effect among endogenous factors }
    \item{\code{TY}:}{Measurement intercepts of Y indicators }
    \item{\code{AL}:}{Factor intercepts of endogenous factors }
    \item{\code{ME}:}{Factor means of endogenous factors }
    \item{\code{MY}:}{Total Mean of Y indicators }
    \item{\code{VE}:}{Total variance of endogenous factors }
    \item{\code{VY}:}{Total variance of Y indicators }
    \item{\code{LX}:}{Factor loading matrix between exogenous factors and X indicators }
    \item{\code{TD}:}{Correlation matrix between X measurement error }
    \item{\code{VTD}:}{Variance of X measurement error }
    \item{\code{PH}:}{Correlation among exogenous factors }
    \item{\code{GA}:}{Regreeion effect from exogenous factors to endogenous factors }
    \item{\code{TX}:}{Measurement intercepts of X indicators }
    \item{\code{KA}:}{Factor Mean of exogenous factors }
    \item{\code{MX}:}{Total Mean of X indicators }
    \item{\code{VPH}:}{Variance of exogenous factors }
    \item{\code{VX}:}{Total variance of X indicators }
    \item{\code{TH}:}{Measurement error correlation between X indicators and Y indicators }
  }
}
\section{Extends}{
Class \code{"\linkS4class{SimSet}"}, directly.
}
\section{Methods}{
  \describe{
    \item{run}{Create a sample of parameters in this object. In other words, draw a sample from all random parameters which is represented in \code{\linkS4class{VirtualDist}}. }
	}
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\enumerate{
		\item Create an object this class by CFA, Path Analysis, or SEM model by \code{\link{simMisspecCFA}}, \code{\link{simMisspecPath}}, or \code{\link{simMisspecSEM}}, respectively.
		\item See how to specify true model by \code{\linkS4class{SimSet}}.
	}
}
\examples{
showClass("SimMisspec")
n01 <- simNorm(0, 0.1)
error.cor.Mis <- matrix(NA, 6, 6)
diag(error.cor.Mis) <- 1
TD.Mis <- symMatrix(error.cor.Mis, "n01")
CFA.Model.Mis <- simMisspecCFA(TD=TD.Mis)
}
\keyword{classes}
