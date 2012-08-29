\name{SimMatrix-class}
\docType{class}
\alias{SimMatrix-class}
\alias{run,SimMatrix-method}
\alias{summaryShort,SimMatrix-method}
\alias{summary,SimMatrix-method}
%\alias{extract,SimMatrix-method}
\title{Matrix object: Random parameters matrix}
\description{
This object can be used to represent a matrix in SEM model. It contains free parameters, fixed values, and starting values. This object can be represented factor loading matrix or regreesion coefficient matrix. 
}
\section{Objects from the Class}{
This object is created by \code{"\link[=simMatrix]{simMatrix}"} function. Objects can be also created by calls of the form \code{new("SimMatrix", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{free}:}{ TBA }
    \item{\code{popParam}:}{ TBA }
    \item{\code{misspec}:}{ TBA }
    \item{\code{symmetric}:}{ TBA }

  }
}
\section{Methods}{
  \describe{
    %\item{\code{\link{adjust}}}{Adjust an element in the \code{"SimMatrix"} object }
    \item{\code{\link{run}}}{Draws starting values from the \code{"labels"} slot and show as a matrix sample. }
    \item{\code{\link{summaryShort}}}{Provides a short summary of all information in the object }
    \item{\code{summary}}{Provides a thorough description of all information in the object }
    %\item{\code{\link{extract}}}{Extract elements from a simMatrix. The additional arguments are the indicies of rows and columns to be extracted. }
	 }
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		%\item \code{\linkS4class{SymMatrix}} for symmetric random parameter matrix 
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
# run(LX)

LY <- bind(loading, "rnorm(1, 0.6, 0.05)")
summary(LY)
# run(LY)
}
\keyword{classes}
