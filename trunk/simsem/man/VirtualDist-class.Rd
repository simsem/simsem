\name{VirtualDist-class}
\Rdversion{1.1}
\docType{class}
\alias{VirtualDist-class}
\alias{SimBeta-class}
\alias{SimBinom-class}
\alias{SimCauchy-class}
\alias{SimChisq-class}
\alias{SimExp-class}
\alias{SimF-class}
\alias{SimGamma-class}
\alias{SimGeom-class}
\alias{SimHyper-class}
\alias{SimLnorm-class}
\alias{SimLogis-class}
\alias{SimNbinom-class}
\alias{SimNorm-class}
\alias{SimPois-class}
\alias{SimT-class}
\alias{SimUnif-class}
\alias{SimWeibull-class}	
\alias{run,SimBeta-method}
\alias{run,SimBinom-method}
\alias{run,SimCauchy-method}
\alias{run,SimChisq-method}
\alias{run,SimExp-method}
\alias{run,SimF-method}
\alias{run,SimGamma-method}
\alias{run,SimGeom-method}
\alias{run,SimHyper-method}
\alias{run,SimLnorm-method}
\alias{run,SimLogis-method}
\alias{run,SimNbinom-method}
\alias{run,SimNorm-method}
\alias{run,SimPois-method}
\alias{run,SimT-method}
\alias{run,SimUnif-method}
\alias{run,SimWeibull-method}
\alias{summary,SimBeta-method}
\alias{summary,SimBinom-method}
\alias{summary,SimCauchy-method}
\alias{summary,SimChisq-method}
\alias{summary,SimExp-method}
\alias{summary,SimF-method}
\alias{summary,SimGamma-method}
\alias{summary,SimGeom-method}
\alias{summary,SimHyper-method}
\alias{summary,SimLnorm-method}
\alias{summary,SimLogis-method}
\alias{summary,SimNbinom-method}
\alias{summary,SimNorm-method}
\alias{summary,SimPois-method}
\alias{summary,SimT-method}
\alias{summary,SimUnif-method}
\alias{summary,SimWeibull-method}
\alias{summaryShort,SimBeta-method}
\alias{summaryShort,SimBinom-method}
\alias{summaryShort,SimCauchy-method}
\alias{summaryShort,SimChisq-method}
\alias{summaryShort,SimExp-method}
\alias{summaryShort,SimF-method}
\alias{summaryShort,SimGamma-method}
\alias{summaryShort,SimGeom-method}
\alias{summaryShort,SimHyper-method}
\alias{summaryShort,SimLnorm-method}
\alias{summaryShort,SimLogis-method}
\alias{summaryShort,SimNbinom-method}
\alias{summaryShort,SimNorm-method}
\alias{summaryShort,SimPois-method}
\alias{summaryShort,SimT-method}
\alias{summaryShort,SimUnif-method}
\alias{summaryShort,SimWeibull-method}	
\alias{plotDist,VirtualDist-method}	
\title{Distribution Objects}
\description{
List of all distribution objects.
}
\section{Distributions}{
	Here is the list of all distribution objects and the link to their constructors.
	\itemize{
		\item \code{\link{simBeta}}
		\item \code{\link{simBinom}}
		\item \code{\link{simCauchy}}
		\item \code{\link{simChisq}}
		\item \code{\link{simExp}}
		\item \code{\link{simF}}
		\item \code{\link{simGamma}}
		\item \code{\link{simGeom}}
		\item \code{\link{simHyper}}
		\item \code{\link{simLnorm}}
		\item \code{\link{simLogis}}
		\item \code{\link{simNbinom}}
		\item \code{\link{simNorm}}
		\item \code{\link{simPois}}
		\item \code{\link{simT}}
		\item \code{\link{simUnif}}
		\item \code{\link{simWeibull}}	
	}
}
\section{Methods}{
  \describe{
    \item{run}{Create a random number from the distribution }
    \item{summary}{Summarize information in the object }
    \item{summaryShort}{Summarize information in the object in a short form }
	\item{plotDist}{Plot a distribution of the distribution object }
	 }
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	List of all distribution objects.
	\itemize{
		\item \code{\linkS4class{SimNorm}}
		\item \code{\linkS4class{SimUnif}}
	}
}
\examples{
showClass("VirtualDist")
}
\keyword{classes}
