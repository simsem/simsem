\name{simMatrix-class}
\Rdversion{1.1}
\docType{class}
\alias{simMatrix-class}
\alias{adjust.object,simMatrix-method}
\alias{run,simMatrix-method}
\alias{summary.short,simMatrix-method}
\alias{summary,simMatrix-method}

\title{Class \code{"simMatrix"} (Random parameters matrix)}
\description{
This object can be used to represent a matrix in SEM model. It contains free parameters, fixed values, and starting values. This object can be represented factor loading matrix or regreesion coefficient matrix. 
}
\section{Objects from the Class}{
This object %is created by \code{"\link[=matrix.object]{matrix.object}"} function. Objects can be also created by calls of the form \code{new("simMatrix", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{Data}:}{ indicates which elements of the matrix are free or fixed. \code{"NA"} means the element is freely estimated. Numbers (including 0) means the element is fixed to be the indicated number.}
    \item{\code{Labels}:}{ indicates the starting values of each element in the matrix. The starting values could be numbers or the name of}  %later \code{"\link[=simDist-class]{distribution objects}"}}

  }
}
\section{Methods}{
  \describe{
    \item{adjust.object}{\code{signature(target = "simMatrix")}: adjust an element in the \code{"simMatrix"} object }
    \item{run}{\code{signature(object = "simMatrix")}: draws starting values from the \code{"labels"} slot and show as a matrix sample. }
    \item{summary.short}{\code{signature(object = "simMatrix")}: provides a short summary of all information in the object }
    \item{summary}{\code{signature(object = "simMatrix")}: provides a thorough description of all information in the object }
	 }
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\seealso{
later %\code{\linkS4class{symMatrix}} for symmetric random parameter matrix and \code{\linkS4class{simVector}} for random parameter vector.
}
\examples{
showClass("simMatrix")

#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#loadingValues[1:3, 1] <- 0.7
#loadingValues[4:6, 2] <- 0.7
#LX <- matrix.object(loading, loadingValues)
#summary(LX)
#run(LX)

#n65 <- rnorm.object(0.6, 0.05)
#LY <- matrix.object(loading, "n65")
#summary(LY)
#run(LY)

#u34 <- runif.object(0.3, 0.4)
#LY <- adjust.object(LY, "u34", c(2, 1))
#summary(LY)
#run(LY)
#summary.short(LY)
}
\keyword{classes}
