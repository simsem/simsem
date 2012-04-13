\name{simParamSEM}
\alias{simParamSEM}
\title{
	Create a set of matrices of parameters for analyzing data that belongs to SEM model
}
\description{
	This function will create set of matrices of free parameters that belongs to full SEM model. The requirement is to specify regression coefficient matrix and factor loading matrix.	
}
\usage{
simParamSEM(..., exo = FALSE)
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
		\item \code{LY} for factor loading matrix from endogenous factors to Y indicators (need to be a matrix). 
		\item \code{TE} for measurement error covariance matrix among Y indicators (need to be a symmetric matrix).
		\item \code{BE} for regression coefficient matrix among endogenous factors (need to be a matrix). 
		\item \code{PS} for residual covariance matrix among endogenous factors (need to be a symmetric matrix).
		\item \code{TY} for measurement intercepts of Y indicators. (need to be a vector).
		\item \code{AL} for endogenous factor intercept (need to be a vector).
	}
	There are two required matrices for the specification in the endogenous side only: LY and BE. 
	If users need to specify exogenous variable too (\code{"exo=TRUE"}), these matrices and vectors are available: 
	\itemize{
		\item \code{LX} for factor loading matrix from exogenous factors to X indicators (need to be a matrix). 
		\item \code{TD} for measurement error covariance matrix among X indicators (need to be a symmetric matrix).
		\item \code{GA} for regression coefficient matrix among exogenous factors (need to be a matrix). 
		\item \code{PH} for residual covariance matrix among exogenous factors (need to be a symmetric matrix).
		\item \code{TX} for measurement intercepts of Y indicators. (need to be a vector).
		\item \code{KA} or \code{MK} for total mean of exogenous factors (need to be a vector).
		\item \code{TH} for measurement error covariance between X measurement error and Y measurement error (need to be a matrix).
	}
	There are four required matrices for the specification in both exogenous and endogenous sides: LY, , BE, LX, and GA. The default specifications if \code{exo=FALSE} are
	\enumerate{
		\item If residual factor covariance is not specified, then (a) all factor variances are free, (b) all exogenous covariances are free, (c) all endogenous covariances are fixed.
		\item If factor means vector is not specified, then the factor means are free.
		\item If error covariance matrix is not specified, the default is to estimate all error variances and not estimate error covariances.
		\item If measurement intercept vector is not specified, the default is to estimate all measurement intercepts.
	}
	The default specifications if \code{exo=TRUE} are
	\enumerate{
		\item If endogenous factor covariance (PS) is not specified, then (a) all endogenous factor variances are free, (b) all endogenous factor covariances are fixed.
		\item If endogenous factors regression coefficient (BE) is not specified, then all coefficients are specified as zero.
		\item If factor means (KA or AL) are not specified, all indictor means are free.
		\item If exogenous factor covariance matrix (PH) is not specified, then the matrix is free in every element.	
		\item If error covariance matrix (TE, TD, or TH) is not specified, the default is to estimate all error variances and not estimate error covariances.
		\item If measurement intercept vector (TX or TY) is not specified, the default is to estimate all measurement intercepts.
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
		\item Use \code{\link{simParamCFA}} to specify CFA model and use \code{\link{simParamPath}} to specify path analysis model.
	}
}
\examples{
loading <- matrix(0, 8, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:8, 3] <- NA
path <- matrix(0, 3, 3)
path[3, 1:2] <- NA
SEM.model <- simParamSEM(BE=path, LY=loading)

loading.X <- matrix(0, 6, 2)
loading.X[1:3, 1] <- NA
loading.X[4:6, 2] <- NA
loading.Y <- matrix(NA, 2, 1)
path.GA <- matrix(NA, 1, 2)
BE <- as.matrix(0)
SEM.Exo.model <- simParamSEM(GA=path.GA, BE=BE, LX=loading.X, LY=loading.Y, exo=TRUE)
}
