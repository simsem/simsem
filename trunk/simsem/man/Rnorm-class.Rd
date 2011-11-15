\name{SimNorm-class}
\Rdversion{1.1}
\docType{class}
\alias{SimNorm-class}
\alias{run,SimNorm-method}
\alias{summary,SimNorm-method}
\title{Class "SimNorm"}
\description{
	Object that create a random number from normal distribution.
}
\section{Objects from the Class}{
The object should be created by \code{\link{simNorm}} function. Objects can be created by calls of the form \code{new("SimNorm", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{mean}:}{Mean of the distribution}
    \item{\code{sd}:}{Standard deviation of the distribution}
  }
}
\section{Extends}{
Class \code{"\linkS4class{VirtualDist}"}, directly.
}
\section{Methods}{
  \describe{
    \item{run}{\code{signature(object = "SimNorm")}: create a random number from the distribution  }
    \item{summary}{\code{signature(object = "SimNorm")}: summarize information in the object  }
	 }
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\code{\linkS4class{VirtualDist}} for other distribution objects.
}
\examples{
showClass("SimNorm")
n2 <- simNorm(0, 0.2)
run(n2)
summary(n2)
}
\keyword{classes}
