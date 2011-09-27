\name{simVector-class}
\Rdversion{1.1}
\docType{class}
\alias{simVector-class}
\alias{adjust.object,simVector-method}
\alias{run,simVector-method}
\alias{summary.short,simVector-method}
\alias{summary,simVector-method}

\title{Class \code{"simVector"} (Random parameters vector)}
\description{
This object can be used to represent a vector in SEM model. It contains free parameters, fixed values, and starting values. This object can be represented mean, intercept, or variance vectors.
}
\section{Objects from the Class}{
This object is created by \code{\link{vector.object}} function. Objects can be created by calls of the form \code{new("simVector", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{Data}:}{Object of class \code{"vector"} draws starting values from the \code{"labels"} slot and show as a vector sample. }
    \item{\code{Labels}:}{Object of class \code{"vector"} provides a thorough description of all information in the object }
  }
}
\section{Methods}{
  \describe{
    \item{adjust.object}{\code{signature(target = "simVector")}: adjust an element in the \code{"simVector"} object }
    \item{run}{\code{signature(object = "simVector")}: draws starting values from the \code{"labels"} slot and show as a vector sample. }
    \item{summary.short}{\code{signature(object = "simVector")}: provides a short summary of all information in the object }
    \item{summary}{\code{signature(object = "simVector")}: provides a thorough description of all information in the object }
	 }
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\seealso{
\code{\linkS4class{simMatrix}} for random parameter matrix and \code{\linkS4class{symMatrix}} for random parameter symmetric matrix.
}
\examples{
showClass("simVector")

factor.mean <- rep(NA, 2)
factor.mean.starting <- c(5, 2)
AL <- vector.object(factor.mean, factor.mean.starting)
run(AL)
summary(AL)
summary.short(AL)

n01 <- rnorm.object(0, 1)
AL <- adjust.object(AL, "n01", 2)
run(AL)
summary(AL)
}
\keyword{classes}
