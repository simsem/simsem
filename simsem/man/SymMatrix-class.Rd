\name{SymMatrix-class}
\docType{class}
\alias{SymMatrix-class}
\alias{run,SymMatrix-method}
\alias{summary,SymMatrix-method}
\title{Symmetric matrix object: Random parameters symmetric matrix}
\description{
This object can be used to represent a symmetric matrix in SEM model. It contains free parameters, fixed values, and starting values. This object can be represented factor correlation or error correlation matrix.
}
\section{Objects from the Class}{
This object is created by \code{"\link[=symMatrix]{symMatrix}"} function.  Objects can be also created by calls of the form \code{new("SymMatrix", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{free}:}{indicates which elements of the matrix are free or fixed. \code{"NA"} means the element is freely estimated. Numbers (including 0) means the element is fixed to be the indicated number.}
    \item{\code{value}:}{indicates the starting values of each element in the matrix. The starting values could be numbers or the name of \code{"\link[=VirtualDist-class]{distribution objects}"}}
  }
}
\section{Extends}{
Class \code{"\linkS4class{SimMatrix}"}, directly.
}
\section{Methods}{
  \describe{
    \item{\code{\link{adjust}}}{Adjust an element in the \code{"SymMatrix"} object }
    \item{\code{\link{run}}}{Draws starting values from the \code{"labels"} slot and show as a symmetric matrix sample.  }
    \item{\code{summary}}{Provides a thorough description of all information in the object }
	 }
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
\code{\linkS4class{SimMatrix}} for random parameter matrix and \code{\linkS4class{SimVector}} for random parameter vector.
}
\examples{
showClass("SymMatrix")

latent.cor <- matrix(NA, 3, 3)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, 0.5)

u46 <- simUnif(0.4, 0.6)
RPH <- adjust(RPH, "u46", c(3,2))
summary(RPH)
summaryShort(RPH)
run(RPH)
}
\keyword{classes}
