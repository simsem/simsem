\name{simParamPath}
\alias{simParamPath}
\title{
	Create a set of matrices of parameters for analyzing data that belongs to Path analysis model
}
\description{
	This function will create set of matrices of free parameters that belongs to path analysis model. The requirement is to specify regression coefficient matrix.	
}
\usage{
simParamPath(..., exo = FALSE)
}
\arguments{
  \item{\dots}{
	Each element of model specification, as described in \code{Details}
}
  \item{exo}{
	specify \code{TRUE} if users wish to specify both exogenous and endogenous indicators.
}
}
\details{
	The matrices and vectors in the endogenous side are
	\itemize{
		\item \code{BE} for regression coefficient matrix (need to be a \code{matrix}). 
		\item \code{PS} for residual covariance matrix (need to be a symmetric \code{matrix}).
		\item \code{AL} for indicator intercept (need to be a \code{vector}).
	}
	The only required matrix for the specification in the endogenous side is BE. If users wish to include the exogenous side in their models, these options are available,
	\itemize{
		\item \code{GA} for regression coefficient matrix from exogenous variable to endogenous variable (need to be a \code{matrix}).  
		\item \code{PH} for exogenous indicator covariance (need to be a symmetric \code{matrix}).
		\item \code{KA} or \code{MK} for exogenous variable mean (need to be a \code{vector}). 
	}
	The only required matrix for the specification with exogenous side is GA. The default specifications if \code{exo=FALSE} are
	\enumerate{
		\item If residual covariance is not specified, then (a) all indicator variances are free, (b) all exogenous covariances are free, (c) all endogenous covariances are fixed.
		\item If indicator means vector is not specified, then the indicator means are free.
	}
	The default specifications if \code{exo=TRUE} are
	\enumerate{
		\item If endogenous indicator covariance (PS) is not specified, then (a) all indicator variances are free, (b) all endogenous indicators covariances are fixed.
		\item If endogenous indicators regression coefficient (BE) is not specified, then all coefficients are specified as zero.
		\item If indicator means (KA or AL) are not specified, all indictor means are free.
		\item If exogenous indicators covariance matrix (PH) is not specified, then the matrix is free in every element.		
	}
}
\value{
	\code{\linkS4class{SimParam}} object that represents the path analysis model free parameters. This will be used for building \code{\linkS4class{SimModel}} later.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item See class \code{\linkS4class{SimParam}} for the free parameters object details. 
		\item Use \code{\link{simParamCFA}} to specify CFA model and use \code{\link{simParamSEM}} to specify full structural equation modeling.
	}
}
\examples{ 
path <- matrix(0, 4, 4)
path[3, 1:2] <- NA
path[4, 3] <- NA
model <- simParamPath(BE=path)

exoPath <- matrix(NA, 3, 2)
model2 <- simParamPath(GA=exoPath, exo=TRUE)
}
