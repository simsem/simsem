\name{Runif-class}
\Rdversion{1.1}
\docType{class}
\alias{Runif-class}
\alias{run,Runif-method}
\alias{summary,Runif-method}

\title{Class "Runif"}
\description{
	Object that create a random number from uniform distribution.
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
The object should be created by \code{runif.object} function. Objects can be created by calls of the form \code{new("Runif", ...)}.
%%  ~~ describe objects here ~~ 
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
	Sunthud Pornprasertmanit (University of Kansas, psunthud@ku.edu)
}

\examples{
showClass("Runif")
u1 <- runif.object(-0.1, 0.1)
run(u1)
summary(u1)
}
\keyword{classes}
