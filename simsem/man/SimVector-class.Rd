\name{SimVector-class}
\Rdversion{1.1}
\docType{class}
\alias{SimVector-class}
\alias{SimVector}
\title{Vector object: Random parameters vector}
\description{
This object can be used to represent a vector in SEM model. It contains free parameters, fixed values, starting values, and model misspecification. This object can be represented mean, intercept, or variance vectors.
}
\section{Objects from the Class}{
This object is created by \code{\link{bind}} function. 
}
\section{Fields}{
  \describe{
    \item{\code{free}:}{The free-parameter vector. Any NA elements or character elements are free. Any numeric elements are fixed as the specified number. If any free elements have the same characters (except NA), the elements are equally constrained.}
    \item{\code{popParam}:}{Real population parameters of the free elements.}
	\item{\code{misspec}:}{Model misspecification that will be added on top of the fixed and real parameters.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{\link{rawDraw}}}{Draws data-generation parameters.}
    \item{\code{\link{summaryShort}}}{Provides a short summary of all information in the object }
    \item{\code{\link{summary}}}{Provides a thorough description of all information in the object }

	 }
}
\author{
	Patrick Miller (Univeristy of Notre Dame; \email{pmille13@nd.edu})
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
\code{\link{SimMatrix}} for random parameter matrix 
}
\examples{
showClass("SimVector")

factor.mean <- rep(NA, 2)
factor.mean.starting <- c(5, 2)
AL <- bind(factor.mean, factor.mean.starting)
rawDraw(AL)
summary(AL)
summaryShort(AL)
}
\keyword{classes}
