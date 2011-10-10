\name{simMisspecifiedSet-class}
\Rdversion{1.1}
\docType{class}
\alias{simMisspecifiedSet-class}
\alias{run,simMisspecifiedSet-method}

\title{Class \code{"simMisspecifiedSet"}}
\description{
Misspecification model added on true model specifiction. This class contains \code{\linkS4class{simVector}}, \code{\linkS4class{simMatrix}}, and \code{\linkS4class{symMatrix}} specifying misspecification.
}
\section{Objects from the Class}{
Object can be created by \code{\link{misspecified.CFA.object}}, \code{\link{misspecified.Path.object}}, or \code{\link{misspecified.SEM.object}}, for CFA, Path analysis, or SEM model, respectively. Objects can be also created by calls of the form \code{new("simMisspecifiedSet", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{Tag}:}{Model type (CFA, Path, or SEM) }
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
Class \code{"\linkS4class{simMatrixSet}"}, directly.
}
\section{Methods}{
  \describe{
    \item{run}{Create a sample of parameters in this object. In other words, draw a sample from all random parameters which is represented in \code{\linkS4class{simDist}}. }
	}
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\enumerate{
		\item Create an object this class by CFA, Path Analysis, or SEM model by \code{\link{misspecified.CFA.object}}, \code{\link{misspecified.Path.object}}, or \code{\link{misspecified.SEM.object}}, respectively.
		\item See how to specify true model by \code{\linkS4class{simMatrixSet}}.
	}
}
\examples{
showClass("simMisspecifiedSet")
n01 <- rnorm.object(0, 0.1)
error.cor.Mis <- matrix(NA, 6, 6)
diag(error.cor.Mis) <- 1
TD.Mis <- sym.matrix.object(error.cor.Mis, "n01")
CFA.Model.Mis <- misspecified.CFA.object(TD=TD.Mis)
}
\keyword{classes}
