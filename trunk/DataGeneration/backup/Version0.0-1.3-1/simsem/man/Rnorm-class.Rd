\name{Rnorm-class}
\Rdversion{1.1}
\docType{class}
\alias{Rnorm-class}
\alias{run,Rnorm-method}
\alias{summary,Rnorm-method}

\title{Class "Rnorm"}
\description{
	Object that create a random number from normal distribution.
}
\section{Objects from the Class}{
The object should be created by \code{rnorm.object} function. Objects can be created by calls of the form \code{new("Rnorm", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{Mean}:}{Mean of the distribution}
    \item{\code{SD}:}{Standard deviation of the distribution}
  }
}
\section{Extends}{
Class \code{"\linkS4class{simDist}"}, directly.
}
\section{Methods}{
  \describe{
    \item{run}{\code{signature(object = "Rnorm")}: create a random number from the distribution  }
    \item{summary}{\code{signature(object = "Rnorm")}: summarize information in the object  }
	 }
}

\author{
	Sunthud Pornprasertmanit (University of Kansas, psunthud@ku.edu)
}

\examples{
showClass("Rnorm")
n2 <- rnorm.object(0, 0.2)
run(n2)
summary(n2)
}
\keyword{classes}
