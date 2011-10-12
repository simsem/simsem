\name{matrix.CFA.object}
\alias{matrix.CFA.object}
\title{
	Create a set of matrix that belongs to CFA model.
}
\description{
	This function will create set of matrix that belongs to confirmatory factor analysis. 
	The requirement is to specify factor loading matrix, factor correlation matrix, and error correlation matrix.	
}
\usage{
matrix.CFA.object(...)
}
\arguments{
  \item{\dots}{ Each element of model specification, as described in \code{Details}
}
}
\details{
	NOTE: CFA object can be either specified in X or Y side.
	\enumerate{
		\item REQUIRED: \code{LX} or \code{LY} for factor loading matrix (need to be \code{\linkS4class{simMatrix}} object). 
		\item REQUIRED: \code{TD} or \code{TE} for measurement error correlation matrix (need to be \code{\linkS4class{symMatrix}} object).
		\item REQUIRED: \code{PH} or \code{PH} for factor correlation matrix (need to be \code{\linkS4class{symMatrix}} object).
		\item \code{VTD} or \code{VTE} for measurement error variance (need to be \code{\linkS4class{simVector}} object).
		\item \code{VX} or \code{VY} for total indicator variance (need to be \code{\linkS4class{simVector}} object). 
			NOTE: Either measurement error variance or indicator variance is specified. Both cannot be simultaneously specified.
		\item \code{VPH}, \code{VPS}, \code{VK}, or \code{VE} for factor total variance (need to be \code{\linkS4class{simVector}} object). 
			NOTE: These four objects will have different meanings in \code{matrix.SEM.object} function.
		\item \code{TX} or \code{TY} for measurement intercepts. (need to be \code{\linkS4class{simVector}} object).
		\item \code{MX} or \code{MY} for overall indicator means. (need to be \code{\linkS4class{simVector}} object). 
			NOTE: Either measurement intercept of indicator mean can be specified. Both cannot be specified simultaneously.
		\item \code{KA}, \code{AL}, \code{MK}, or \code{ME} for factor means (need to be \code{\linkS4class{simVector}} object).
	}
	DEFAULT:
	\itemize{
		\item 1) All indicator variances are equal to 1. Measurement error variances are automatically implied from total indicator variances.
		\item 2) All measurement error variances are free parameters.
		\item 3) All indicator means are equal to 0. Indicator intercepts are automatically implied from indicator means.
		\item 4) All indicator intercepts are free parameters.
		\item 5) All factor variances are equal to 1.
		\item 6) All factor variances are fixed.
		\item 7) All factor means are equal to 0.
		\item 8) All factor means are fixed.
	}
}
\value{
	\code{\linkS4class{simMatrixSet}} object that represents the CFA object. This will be used for specifying data or model objects later.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\enumerate{
		\item See class \code{\linkS4class{simMatrixSet}} for result object details. 
		\item See \code{\linkS4class{simMatrix}}, \code{\linkS4class{symMatrix}}, or \code{\linkS4class{simVector}} for input details.
		\item Use \code{\link{matrix.Path.object}} to specify path analysis model and use \code{\link{matrix.SEM.object}} to specify full structural equation modeling.
	}
}
\examples{
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- matrix.object(loading, loadingValues)
summary(LX)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
PH <- sym.matrix.object(latent.cor, 0.5)

error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
TD <- sym.matrix.object(error.cor)

CFA.Model <- matrix.CFA.object(LX = LX, PH = PH, TD = TD)
}
