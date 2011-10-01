\name{simMatrixSet-class}
\Rdversion{1.1}
\docType{class}
\alias{simMatrixSet-class}
\alias{summary,simMatrixSet-method}

\title{Class \code{"simMatrixSet"}}
\description{
	Set of vectors and matrices that saves model specification (CFA, Path analysis, or SEM)
}
\section{Objects from the Class}{
Object can be created by \code{\link{matrix.Path.object}}, \code{\link{matrix.Path.object}}, or \code{\link{matrix.Path.object}}, for CFA, Path analysis, or SEM model, respectively. Objects can be also created by calls of the form \code{new("simMatrixSet", ...)}.
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
\section{Methods}{
  \describe{
    \item{summary}{Get the summary of model specification }
	 }
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\seealso{
	Create this class by CFA, Path Analysis, or SEM model by \code{\link{matrix.Path.object}}, \code{\link{matrix.Path.object}}, or \code{\link{matrix.Path.object}}, respectively.
}
\examples{
showClass("simMatrixSet")

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- matrix.object(loading, loadingValues)
summary(LX)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
PH <- sym.matrix.object(latent.cor, 0.5)

# Error Correlation Object
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
TD <- sym.matrix.object(error.cor)

CFA.Model <- matrix.CFA.object(LX = LX, PH = PH, TD = TD)
summary(CFA.Model)
#run(CFA.Model)
}
