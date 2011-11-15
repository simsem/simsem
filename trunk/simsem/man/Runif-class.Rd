\name{SimUnif-class}
\Rdversion{1.1}
\docType{class}
\alias{SimUnif-class}
\alias{run,SimUnif-method}
\alias{summary,SimUnif-method}

\title{Class "SimUnif"}
\description{
	Object that create a random number from uniform distribution.
}
\section{Objects from the Class}{
The object should be created by \code{\link{simUnif}} function. Objects can be created by calls of the form \code{new("SimUnif", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{min}:}{ Lower bound parameter }
    \item{\code{max}:}{ Upper bound parameter }
  }
}
\section{Extends}{
Class \code{"\linkS4class{VirtualDist}"}, directly.
}
\section{Methods}{
  \describe{
    \item{run}{\code{signature(object = "SimUnif")}: create a random number from the distribution }
    \item{summary}{\code{signature(object = "SimUnif")}: summarize information in the object }
	 }
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\code{\linkS4class{VirtualDist}} for other distribution objects.
}
\examples{
showClass("SimUnif")
u1 <- simUnif(-0.1, 0.1)
run(u1)
summary(u1)
}
\keyword{classes}
