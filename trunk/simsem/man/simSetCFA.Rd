\name{simSetCFA}
\alias{simSetCFA}
\title{
	Create a set of matrix that belongs to CFA model.
}
\description{
	This function will create set of matrix that belongs to confirmatory factor analysis. 
	The requirement is to specify factor loading matrix, factor correlation matrix, and error correlation matrix.	
}
\usage{
simSetCFA(...)
}
\arguments{
  \item{\dots}{ Each element of model specification, as described in \code{Details}
}
}
\details{
	NOTE: CFA object can be either specified in X or Y side.
	\itemize{
		\item REQUIRED: \code{LX} or \code{LY} for factor loading matrix (need to be \code{\linkS4class{SimMatrix}} object). 
		\item REQUIRED: \code{TD} or \code{TE} for measurement error correlation matrix (need to be \code{\linkS4class{SymMatrix}} object).
		\item REQUIRED: \code{PH} or \code{PH} for factor correlation matrix (need to be \code{\linkS4class{SymMatrix}} object).
		\item \code{VTD} or \code{VTE} for measurement error variance (need to be \code{\linkS4class{SimVector}} object).
		\item \code{VX} or \code{VY} for total indicator variance (need to be \code{\linkS4class{SimVector}} object). 
			NOTE: Either measurement error variance or indicator variance is specified. Both cannot be simultaneously specified.
		\item \code{VPH}, \code{VPS}, \code{VK}, or \code{VE} for factor total variance (need to be \code{\linkS4class{SimVector}} object). 
			NOTE: These four objects will have different meanings in \code{simSetSEM} function.
		\item \code{TX} or \code{TY} for measurement intercepts. (need to be \code{\linkS4class{SimVector}} object).
		\item \code{MX} or \code{MY} for overall indicator means. (need to be \code{\linkS4class{SimVector}} object). 
			NOTE: Either measurement intercept of indicator mean can be specified. Both cannot be specified simultaneously.
		\item \code{KA}, \code{AL}, \code{MK}, or \code{ME} for factor means (need to be \code{\linkS4class{SimVector}} object).
	}
	DEFAULT:
	\enumerate{
		\item All indicator variances are equal to 1. Measurement error variances are automatically implied from total indicator variances.
		\item All measurement error variances are free parameters.
		\item All indicator means are equal to 0. Indicator intercepts are automatically implied from indicator means.
		\item All indicator intercepts are free parameters.
		\item All factor variances are equal to 1.
		\item All factor variances are fixed.
		\item All factor means are equal to 0.
		\item All factor means are fixed.
	}
}
\value{
	\code{\linkS4class{SimSet}} object that represents the CFA object. This will be used for specifying data or simModels later.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item See class \code{\linkS4class{SimSet}} for simResult details. 
		\item See \code{\linkS4class{SimMatrix}}, \code{\linkS4class{SymMatrix}}, or \code{\linkS4class{SimVector}} for input details.
		\item Use \code{\link{simSetPath}} to specify path analysis model and use \code{\link{simSetSEM}} to specify full structural equation modeling.
	}
}
\examples{
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- simMatrix(loading, loadingValues)
summary(LX)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
PH <- symMatrix(latent.cor, 0.5)

error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
TD <- symMatrix(error.cor)

CFA.Model <- simSetCFA(LX = LX, PH = PH, TD = TD)
}
