\name{SimGenLabels-class}
\Rdversion{1.1}
\docType{class}
\alias{SimGenLabels-class}
\alias{run,SimGenLabels-method}

\title{Class \code{"SimGenLabels"}}
\description{
The label of parameter values in the data generation parameterization format.
}
\section{Objects from the Class}{
Object can be created by \code{\link{makeLabels}}, Objects can be also created by calls of the form \code{new("SimGenLabels", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{modelType}:}{Model type (CFA, Path, or SEM) }
    \item{\code{LY}:}{The labels of the factor loading matrix between endogenous factors and Y indicators }
    \item{\code{TE}:}{The labels of the covariance matrix between Y measurement error }
    \item{\code{RTE}:}{The labels of the correlation matrix between Y measurement error }
    \item{\code{VTE}:}{The labels of the variance of Y measurement error }
    \item{\code{PS}:}{The labels of the residual covariance of endogenous factors  }
    \item{\code{RPS}:}{The labels of the residual correlation of endogenous factors  }
    \item{\code{VPS}:}{The labels of the residual variances of endogenous factors }
    \item{\code{BE}:}{The labels of the regression effect among endogenous factors }
    \item{\code{TY}:}{The labels of the measurement intercepts of Y indicators }
    \item{\code{AL}:}{The labels of the factor intercepts of endogenous factors }
    \item{\code{ME}:}{The labels of the factor means of endogenous factors }
    \item{\code{MY}:}{The labels of the total Mean of Y indicators }
    \item{\code{VE}:}{The labels of the total variance of endogenous factors }
    \item{\code{VY}:}{The labels of the total variance of Y indicators }
    \item{\code{LX}:}{The labels of the factor loading matrix between exogenous factors and X indicators }
    \item{\code{TD}:}{The labels of the covariance matrix between X measurement error }
    \item{\code{RTD}:}{The labels of the correlation matrix between X measurement error }
    \item{\code{VTD}:}{The labels of the variance of X measurement error }
    \item{\code{PH}:}{The labels of the covariance among exogenous factors }
    \item{\code{RPH}:}{The labels of the correlation among exogenous factors }
    \item{\code{GA}:}{The labels of the regreeion effect from exogenous factors to endogenous factors }
    \item{\code{TX}:}{The labels of the measurement intercepts of X indicators }
    \item{\code{KA}:}{The labels of the factor Mean of exogenous factors }
    \item{\code{MX}:}{The labels of the total Mean of X indicators }
    \item{\code{VPH}:}{The labels of the variance of exogenous factors }
    \item{\code{VX}:}{The labels of the total variance of X indicators }
    \item{\code{TH}:}{The labels of the measurement error covariance between X indicators and Y indicators }
    \item{\code{RTH}:}{The labels of the measurement error correlation between X indicators and Y indicators }
	}
}
\section{Extends}{
Class \code{"\linkS4class{MatrixSet}"}, directly.
}
\section{Methods}{
  \describe{
    \item{summary}{Get the summary of model specification }
	 }
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{MatrixSet}}
	}
}
\examples{
# No example
}
\keyword{classes}
