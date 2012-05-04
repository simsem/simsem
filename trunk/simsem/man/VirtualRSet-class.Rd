\name{ParameterSet}
\Rdversion{1.1}
\docType{class}
\alias{VirtualRSet-class}
\alias{SimRSet-class}
\alias{SimLabels-class}
\alias{summary,VirtualRSet-method}
\alias{summary,SimRSet-method}
\alias{summary,SimLabels-method}

\title{Class \code{"VirtualRSet"}, \code{"SimLabels"} and \code{"SimRSet"}}
\description{
	\code{"SimRSet"} is the set of vectors and matrices arrangements that will save values that will be used for various purposes. \code{"SimLabels"} is set of vectors and matrices arrangements that will save labels that will be used for various purposes. \code{"VirtualRSet"} is the superclass of the \code{"SimRSet"}, \code{\linkS4class{SimLabels}}, and \code{\linkS4class{SimParam}}.
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
}
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimParam}}
	}
}
\examples{
# No example
}
