\name{simData-class}
\Rdversion{1.1}
\docType{class}
\alias{simData-class}
\alias{run,simData-method}
\alias{summary,simData-method}
\title{Class \code{"simData"}}
\description{
This class will save information for data simulation and can create data by run function.
}
\section{Objects from the Class}{
Objects can be created by \code{\link{data.object}}. Also, it can be called by \code{new("simData", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{Tag}:}{Model type (CFA, Path, or SEM)}
    \item{\code{N}:}{Sample size  }
    \item{\code{Parameters}:}{Model specification that used in data generation. It must be in \code{\linkS4class{simMatrixSet}} class.}
    \item{\code{Misspecified}:}{Model misspecification that used in data generation. It must be in \code{\linkS4class{simMisspecifiedSet}} class.}
    \item{\code{Constraint}:}{Equality constraints in data generation. It must be in \code{\linkS4class{simConstraint}} class.}
    \item{\code{Constrain.Parameters.Only}:}{\code{TRUE} if users wish to constrain parameters before adding misspecification. \code{FALSE} if users wish to constrain parameters after adding misspecification. }
    \item{\code{Misfit.bound}:}{Upper bound of population RMSEA that users wish their model misspecification to be}
    \item{\code{Maximum.random}:}{The maximum number of random drawn parameters and misspecification model until all parameters in the model are eligible (no negative error variance, standardized coefficients over 1).}
  }
}
\section{Methods}{
  \describe{
    \item{run}{To create data from this class. \code{N} is the additional argument that users may change the sample size when creating data.}
    \item{summary}{Summarize all attributes in the data object.}
	 }
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\enumerate{
		\item \code{linkS4class{simMatrixSet}} for how to specify data generation model.
		\item \code{linkS4class{simMisspecifiedSet}} for how to specify misspecification in this data generation model. 
		\item \code{linkS4class{simConstraint}} for how to set equality constraints for data generation.
		\item \code{link{result.object}} for the use of this class to run Monte Carlo simulation.
	}
}
\examples{
showClass("simData")
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- matrix.object(loading, loadingValues)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
PH <- sym.matrix.object(latent.cor, 0.5)
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
TD <- sym.matrix.object(error.cor)
CFA.Model <- matrix.CFA.object(LY = LX, PS = PH, TE = TD)
SimData <- data.object(200, CFA.Model)
summary(SimData)
run(SimData)
}
\keyword{classes}
