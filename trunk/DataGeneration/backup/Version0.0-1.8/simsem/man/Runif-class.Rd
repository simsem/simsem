\name{Runif-class}
\Rdversion{1.1}
\docType{class}
\alias{Runif-class}
\alias{run,Runif-method}
\alias{summary,Runif-method}

\title{Class "Runif"}
\description{
	Object that create a random number from uniform distribution.
}
\section{Objects from the Class}{
The object should be created by \code{\link{runif.object}} function. Objects can be created by calls of the form \code{new("Runif", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{Lower}:}{ Lower bound parameter }
    \item{\code{Upper}:}{ Upper bound parameter }
  }
}
\section{Extends}{
Class \code{"\linkS4class{simDist}"}, directly.
}
\section{Methods}{
  \describe{
    \item{run}{\code{signature(object = "Runif")}: create a random number from the distribution }
    \item{summary}{\code{signature(object = "Runif")}: summarize information in the object }
	 }
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\code{\linkS4class{simDist}} for other distribution objects.
}
\examples{
showClass("Runif")
u1 <- runif.object(-0.1, 0.1)
run(u1)
summary(u1)
}
\keyword{classes}
