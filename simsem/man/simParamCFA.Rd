\name{simParamCFA}
\alias{simParamCFA}
\title{
	Create a set of matrices of parameters for analyzing data that belongs to CFA model.
}
\description{
	This function will create set of matrices of free parameters that belongs to confirmatory factor analysis. 
	The requirement is to specify factor loading matrix.	
}
\usage{
simParamCFA(...)
}
\arguments{
  \item{\dots}{ Each element of model specification, as described in \code{Details}
}
}
\details{
	NOTE: CFA object can be either specified in X or Y side.
	\itemize{
		\item \code{LX} or \code{LY} for factor loading matrix (need to be a \code{matrix}). 
		\item \code{TD} or \code{TE} for measurement error covariance matrix (need to be a \code{matrix}).
		\item \code{PH} or \code{PS} for factor covariance matrix (need to be a symmetric \code{matrix}).
		\item \code{TX} or \code{TY} for measurement intercepts (need to be a \code{vector}).
		\item \code{KA}, \code{AL}, \code{MK}, or \code{ME} for factor means (need to be a \code{vector}).
	}
	There are only one required matrices: LY (or LX). The default specifications are
	\enumerate{
		\item The scale-identification default of this model is fixed factor method (factor variances = 1 and factor means = 0).
		\item If error covariance matrix is not specified, the default is to estimate all error variances and not estimate error covariances.
		\item If factor covariance matrix is not specified, the default is to fix all factor covariance.
		\item If factor mean vector is not specified, the default is to fix all factor means to 0.
		\item If measurement intercept vector is not specified, the default is to estimate all measurement intercepts.
	}
}
\value{
	\code{\linkS4class{SimParam}} object that represents the CFA free parameters. This will be used for building \code{\linkS4class{SimModel}} later.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item See class \code{\linkS4class{SimParam}} for the free parameters object details. 
		\item Use \code{\link{simParamPath}} to specify path analysis model and use \code{\link{simParamSEM}} to specify full structural equation modeling.
	}
}
\examples{
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
CFA.Model <- simParamCFA(LX = loading)
}
