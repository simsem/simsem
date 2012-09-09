\name{SimMatrix-class}
\docType{class}
\alias{SimMatrix-class}
\alias{summaryShort,SimMatrix-method}
\alias{summary,SimMatrix-method}
\title{Matrix object: Random parameters matrix}
\description{
This object can be used to represent a matrix in SEM model. It contains free parameters, fixed values, starting values, and model misspecification. This object can be represented mean, intercept, or variance vectors.
}
\section{Objects from the Class}{
This object is created by \code{\link{bind}} or \code{\link{binds}} function. 
}
\section{Slots}{
  \describe{
    \item{\code{free}:}{ The free-parameter vector. Any NA elements or character elements are free. Any numeric elements are fixed as the specified number. If any free elements have the same characters (except NA), the elements are equally constrained. }
    \item{\code{popParam}:}{ Real population parameters of the free elements. }
    \item{\code{misspec}:}{ Model misspecification that will be added on top of the fixed and real parameters. }
    \item{\code{symmetric}:}{ If TRUE, the specified matrix is symmetric. }

  }
}
\section{Methods}{
  \describe{
    \item{\code{\link{rawDraw}}}{Draws data-generation parameters.}
    \item{\code{\link{summaryShort}}}{Provides a short summary of all information in the object }
    \item{\code{summary}}{Provides a thorough description of all information in the object }
	 }
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimVector}} for random parameter vector.
	}
}
\examples{
showClass("SimMatrix")

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- bind(loading, loadingValues)
summary(LX)
rawDraw(LX)

LY <- bind(loading, "rnorm(1, 0.6, 0.05)")
summary(LY)
rawDraw(LY)
}
\keyword{classes}
