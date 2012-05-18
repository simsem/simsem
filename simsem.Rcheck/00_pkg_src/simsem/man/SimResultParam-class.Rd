\name{SimResultParam-class}
\Rdversion{1.1}
\docType{class}
\alias{SimResultParam-class}
\alias{summary,SimResultParam-method}
\alias{summaryParam,SimResultParam-method}

\title{Class \code{"SimResultParam"}}
\description{
The parameter result object that represents the parameter values used in each replication in a simulation study. This object saves the parameter values and model misspecification of each replication and is able to summarize the population misfits.
}
\section{Objects from the Class}{
Object can be created by \code{\link{simResultParam}}, Objects can be also created by calls of the form \code{new("SimResultParam", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{modelType}:}{Model type (CFA, Path, Path.exo, SEM, or SEM.exo) }
    \item{\code{nRep}:}{The number of drawn (replications)}
    \item{\code{param}:}{The \code{\linkS4class{SimSet}} class that saves the specification of actual parameters }
    \item{\code{misspec}:}{The \code{\linkS4class{SimMisspec}} class that saves the specification of misspecified parameters }
    \item{\code{fit}:}{The population misfits }
    \item{\code{seed}:}{The seed number used to draw parameters  }
	}
}
\section{Methods}{
  \describe{
    \item{summary}{Get the summary of model specification }
	\item{summaryParam}{Get the summary of the obtained actual parameters}
	\item{plotMisfit}{Plot the population misfit or the relationship between the amount of misfit and the misspecified parameters. See \code{\link{plotMisfit}} for further details.}
	 }
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\link{simResultParam}} for the constructor this class
		\item \code{\linkS4class{SimSet}} for the specification of model parameter
		\item \code{\linkS4class{SimMisspec}} for the specification of misspecified parameter
		\item \code{\linkS4class{SimEqualCon}} for the specification of the equality constraints
	}
}
\examples{
showClass("SimResultParam")

u35 <- simUnif(0.3, 0.5)
u57 <- simUnif(0.5, 0.7)
u1 <- simUnif(-0.1, 0.1)
n31 <- simNorm(0.3, 0.1)

path.BE <- matrix(0, 4, 4)
path.BE[3, 1:2] <- NA
path.BE[4, 3] <- NA
starting.BE <- matrix("", 4, 4)
starting.BE[3, 1:2] <- "u35"
starting.BE[4, 3] <- "u57"
BE <- simMatrix(path.BE, starting.BE)

residual.error <- diag(4)
residual.error[1,2] <- residual.error[2,1] <- NA
RPS <- symMatrix(residual.error, "n31")

ME <- simVector(rep(NA, 4), 0)

Path.Model <- simSetPath(RPS = RPS, BE = BE, ME = ME)

mis.path.BE <- matrix(0, 4, 4)
mis.path.BE[4, 1:2] <- NA
mis.BE <- simMatrix(mis.path.BE, "u1")
Path.Mis.Model <- simMisspecPath(BE = mis.BE, misfitType="rmsea") #, misfitBound=c(0.05, 0.08))

# The number of replications in actual analysis should be much more than 50
ParamObject <- simResultParam(50, Path.Model, Path.Mis.Model)
summary(ParamObject)
}
\keyword{classes}
