\name{popMisfit}
\alias{popMisfit}
\alias{popMisfit-methods}
\alias{popMisfit,ANY,ANY-method}
\alias{popMisfit,matrix,matrix-method}
\alias{popMisfit,list,list-method}
\alias{popMisfit,SimRSet,SimRSet-method}
\alias{popMisfit,MatrixSet,MatrixSet-method}
\alias{popMisfit,SimSet,SimMisspec-method}
\title{
	Calculate population misfit
}
\description{
	Calculate population misfit given the types of object
}
\usage{
popMisfit(param, misspec, dfParam=NULL, fit.measures="all", ...)
}
\arguments{
  \item{param}{
	The object represents the actual parameters
}
  \item{misspec}{
	The object represents the misspecified parameters
}
  \item{dfParam}{
	The degree of freedoms of the target model
}
  \item{fit.measures}{
	The type of population misfit measures: \code{"F0"}, \code{"rmsea"}, or \code{"srmr"}. See \code{\link{popMisfitMACS}} for further details.
}
  \item{\dots}{
	The additional arguments
}
}
\section{Methods}{
	\describe{
	\item{signature(param = "matrix", misspec = "matrix")}{
		Calculate population misfit using actual covariance matrices with and without misspecification		
	}
	\item{signature(param = "list", misspec = "list")}{
		Calculate population misfit from two lists of sufficient statistics with and without misspecification. The list should have the first argument as the mean vector and the second argument as the covariance matrix.	
	}
	\item{signature(param = "SimRSet", misspec = "SimRSet")}{
		Calculate population misfit from two set of parameters with and without misspecification.
	}
	\item{signature(param = "MatrixSet", misspec = "MatrixSet")}{
		Calculate population misfit from two set of parameters with and without misspecification.
	}
	\item{signature(param = "SimSet", misspec = "SimMisspec")}{
		Calculate population misfit from the set of actual parameter specification and the set of misspecified parameter specification.
	}
}}
\value{
	The value of population misfit
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
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

popMisfit(Path.Model, Path.Mis.Model, fit.measures="rmsea")
}
