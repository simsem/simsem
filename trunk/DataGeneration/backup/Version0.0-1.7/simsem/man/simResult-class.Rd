\name{simResult-class}
\Rdversion{1.1}
\docType{class}
\alias{simResult-class}
\alias{summary,simResult-method}

\title{Class \code{"simResult"}}
\description{
This class will save data analysis results from multiple replications and ready to find some useful statistics, such as fit indices cutoffs or power.
}
\section{Objects from the Class}{
Objects can be created by \code{\link{result.object}}. It can also be called from the form \code{new("simResult", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{Tag}:}{Analysis model type (CFA, Path, or SEM) }
    \item{\code{Data}:}{Data object that save data generation model. It must be in \code{\linkS4class{simData}} class. }
    \item{\code{Model}:}{Model object that save analysis model. It must be in \code{\linkS4class{simModel}} class. }
    \item{\code{Replication}:}{Number of replications have been created and run simulated data. }
    \item{\code{Output}:}{Fit Indices values from each replication}
    \item{\code{Convergence}:}{Number of convergence replications }
    \item{\code{Seed}:}{Seed number. }
  }
}
\section{Methods}{
	\enumerate{
		\item \code{\link{find.cutoff}} to find cutoff of fit indices based on a priori alpha level.
		\item \code{\link{find.power}} to find power of rejection when the result object is the alternative hypothesis and users specify cutoffs of the fit indices.
		\item \code{\link{plot.cutoff}} to plot null hypothesis sampling distributions of fit indices with an option to draw fit indices cutoffs by specifying a priori alpha level.
		\item \code{\link{plot.power}} to plot alternative hypothesis (and null hypothesis) with a priori cutoffs or alpha level.
		\item \code{\link{summary}} to summarize the result output
	}
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\enumerate{
		\item \code{\linkS4class{simData}} for data generation model.
		\item \code{\linkS4class{simModel}} for analysis model
		\item \code{\link{result.object}} for the constructor of this class
	
	}
}
\examples{
showClass("simResult")
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- matrix.object(loading, 0.7)
PH <- sym.matrix.object(diag(1))
TD <- sym.matrix.object(diag(6))
CFA.Model <- matrix.CFA.object(LY = LX, PS = PH, TE = TD)
SimData <- data.object(500, CFA.Model)
SimModel <- model.object(CFA.Model)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
Output <- result.object(SimData, SimModel, 50)
summary(Output)
find.cutoff(Output, 0.95)
}
\keyword{classes}
