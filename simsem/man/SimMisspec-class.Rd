\name{SimMisspec-class}
\Rdversion{1.1}
\docType{class}
\alias{SimMisspec-class}
\alias{run,SimMisspec-method}
\alias{summary,SimMisspec-method}

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
    \item{\code{GA}:}{Regreeion effect from exogenous factors to endogenous factors }
    \item{\code{TX}:}{Measurement intercepts of X indicators }
    \item{\code{KA}:}{Factor Mean of exogenous factors }
    \item{\code{MX}:}{Total Mean of X indicators }
    \item{\code{VPH}:}{Variance of exogenous factors }
    \item{\code{VX}:}{Total variance of X indicators }
    \item{\code{TH}:}{Measurement error covariance between X indicators and Y indicators }
    \item{\code{RTH}:}{Measurement error correlation between X indicators and Y indicators }
    \item{\code{conBeforeMis}:}{\code{TRUE} if users wish to constrain parameters before adding misspecification. \code{FALSE} if users wish to constrain parameters after adding misspecification.}
    \item{\code{misBeforeFill}:}{\code{TRUE} if users wish to apply misspecification before applying the auto-completion on the parameters that users have not specified. \code{FALSE} if users wish to apply the auto-completion before adding misspecification. This option is helpful when users wish to apply misspecification on the parameters that users have not specified (e.g., adding trivial misspecification on the residual variance, which users let the package to calculate it and not specify it). See \code{\link{runMisspec}} for further details.}
    \item{\code{misfitType}:}{The type of population misfit used in the \code{misfitBound} below. The default is \code{"rmsea"}. The two other options are \code{"f0"} and \code{"srmr"}. See \code{\link{popMisfitMACS}} for further details.}
    \item{\code{misfitBound}:}{The lower and upper bounds of the population misfit. This option must be a vector with two elements. }
    \item{\code{averageNumMisspec}:}{If \code{TRUE}, the misfit will be divided by the number of free elements in the misspecification object. The default is \code{FALSE}.}
    \item{\code{optMisfit}:}{Use the optimization method to pick the misspecification set. That is, the program will draw a number of misspecification sets. Then, the different sets of misspecification will be compared together. If \code{"min"} is specified, the program will pick the misspecification set the provides the least amount of misfit. If \code{"max"} is specified, the program will pick the set that has the largets misfit. The default is \code{"none"} to not use the optimization method.}
    \item{\code{numIter}:}{The number of different misspecification sets for comparison in the optimization method.}
	}
}
\section{Extends}{
Class \code{"\linkS4class{SimSet}"}, directly.
}
\section{Methods}{
  \describe{
    \item{summary}{Provide the brief description of this object. }
    \item{run}{Create a sample of parameters in this object. In other words, draw a sample from all random parameters which is represented in \code{\linkS4class{VirtualDist}}. }
	}
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item Create an object this class by CFA, Path Analysis, or SEM model by \code{\link{simMisspecCFA}}, \code{\link{simMisspecPath}}, or \code{\link{simMisspecSEM}}, respectively.
		\item See how to specify true model by \code{\linkS4class{SimSet}}.
	}
}
\examples{
showClass("SimMisspec")
n01 <- simNorm(0, 0.1)
error.cor.Mis <- matrix(NA, 6, 6)
diag(error.cor.Mis) <- 1
RTD.Mis <- symMatrix(error.cor.Mis, "n01")
CFA.Model.Mis <- simMisspecCFA(RTD=RTD.Mis)
}
\keyword{classes}
