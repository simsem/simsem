\name{nullSimMatrix-class}
\Rdversion{1.1}
\docType{class}
\alias{nullSimMatrix-class}

\title{Class \code{"nullSimMatrix"} (Random parameters symmetric matrix)}
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
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\seealso{
\code{\linkS4class{simMatrix}} for random parameter matrix and \code{\linkS4class{simVector}} for random parameter vector.
}

