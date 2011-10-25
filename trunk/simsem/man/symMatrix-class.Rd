\name{symMatrix-class}
\Rdversion{1.1}
\docType{class}
\alias{symMatrix-class}
\alias{run,symMatrix-method}
\alias{summary,symMatrix-method}
\title{Class \code{"symMatrix"} (Random parameters symmetric matrix)}
\description{
This object can be used to represent a symmetric matrix in SEM model. It contains free parameters, fixed values, and starting values. This object can be represented factor correlation or error correlation matrix.
}
\section{Objects from the Class}{
This object is created by \code{"\link[=sym.matrix.object]{sym.matrix.object}"} function.  Objects can be also created by calls of the form \code{new("symMatrix", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{Data}:}{indicates which elements of the matrix are free or fixed. \code{"NA"} means the element is freely estimated. Numbers (including 0) means the element is fixed to be the indicated number.}
    \item{\code{Labels}:}{indicates the starting values of each element in the matrix. The starting values could be numbers or the name of \code{"\link[=simDist-class]{distribution objects}"}}
  }
}
\section{Extends}{
Class \code{"\linkS4class{simMatrix}"}, directly.
}
\section{Methods}{
  \describe{
    \item{adjust.object}{\code{signature(target = "symMatrix")}: adjust an element in the \code{"symMatrix"} object }
    \item{run}{\code{signature(object = "symMatrix")}: draws starting values from the \code{"labels"} slot and show as a symmetric matrix sample.  }
    \item{summary}{\code{signature(object = "symMatrix")}: provides a thorough description of all information in the object }
	 }
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
\code{\linkS4class{simMatrix}} for random parameter matrix and \code{\linkS4class{simVector}} for random parameter vector.
}
\examples{
showClass("symMatrix")

latent.cor <- matrix(NA, 3, 3)
diag(latent.cor) <- 1
PH <- sym.matrix.object(latent.cor, 0.5)

u46 <- runif.object(0.4, 0.6)
PH <- adjust.object(PH, "u46", c(3,2))
summary(PH)
summary.short(PH)
run(PH)
}
\keyword{classes}
