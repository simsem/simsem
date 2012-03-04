\name{simSetPath}
\alias{simSetPath}
\title{
	Create a set of matrix belongs to Path analysis model
}
\description{
	This function will create set of matrix that belongs to path analysis model. 
	The requirement is to specify indicator correlation and regression coefficient matrix.	
}
\usage{
simSetPath(..., exo = FALSE)
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
		\item REQUIRED: \code{BE} for regression coefficient matrix (need to be \code{\linkS4class{SimMatrix}} object). 
		\item REQUIRED: \code{RPS} for residual correlation matrix (need to be \code{\linkS4class{SymMatrix}} object).
		\item \code{VPS} for residual indicator variance (need to be \code{\linkS4class{SimVector}} object).
		\item \code{VE} for total indicator variance (need to be \code{\linkS4class{SimVector}} object). NOTE: Either total indicator variance or residual indicator variance is specified. Both cannot be simultaneously specified.
		\item \code{AL} for indicator intercept (need to be \code{\linkS4class{SimVector}} object).
		\item \code{ME} for indicator total mean (need to be \code{\linkS4class{SimVector}} object). NOTE: Either indicator intercept or indicator total mean is specified. Both cannot be simultaneously specified.
		\item \code{VPS} for residual indicator variance (need to be \code{\linkS4class{SimVector}} object).
		\item \code{VE} for total indicator variance (need to be \code{\linkS4class{SimVector}} object). NOTE: Either total indicator variance or residual indicator variance is specified. Both cannot be simultaneously specified.
		\item \code{AL} for indicator intercept (need to be \code{\linkS4class{SimVector}} object).
		\item \code{ME} for indicator total mean (need to be \code{\linkS4class{SimVector}} object). NOTE: Either indicator intercept or indicator total mean is specified. Both cannot be simultaneously specified.
	}
	If users wish to include the exogenous side in their models, these options are available,
	\itemize{
		\item REQUIRED for \code{"exo=TRUE"}: \code{GA} for regression coefficient matrix from exogenous variable to endogenous variable (need to be \code{\linkS4class{SimMatrix}} object).  
		\item REQUIRED for \code{"exo=TRUE"}: \code{RPH} for exogenous factor correlation (need to be \code{\linkS4class{SymMatrix}} object).
		\item \code{VPH} or \code{VK} for exogenous variable variance (need to be \code{\linkS4class{SimVector}} object).
		\item \code{KA} or \code{MK} for exogenous variable mean (need to be \code{\linkS4class{SimVector}} object). NOTE: Either total indicator variance or residual indicator variance is specified. Both cannot be simultaneously specified.
	}
	DEFAULT:
	\enumerate{
		\item All indicator variances are equal to 1. Residual variances are automatically implied from total indicator variances.
		\item All residual variances are free parameters.
		\item All indicator means are equal to 0. Intercepts are automatically implied from total indicator mean.
		\item All indicator intercepts are free parameters.
	}
}
\value{
	\code{\linkS4class{SimSet}} object that represents the path analysis simModel. This will be used for specifying data or simModels later.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item See class \code{\linkS4class{SimSet}} for simResult details. 
		\item See \code{\linkS4class{SimMatrix}}, \code{\linkS4class{SymMatrix}}, or \code{\linkS4class{SimVector}} for input details.
		\item Use \code{\link{simSetCFA}} to specify CFA model and use \code{\link{simSetSEM}} to specify full structural equation modeling.
	}
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

Path.Model <- simSetPath(RPS = RPS, BE = BE)

u35 <- simUnif(0.3, 0.5)
u57 <- simUnif(0.5, 0.7)
u1 <- simUnif(-0.1, 0.1)
n31 <- simNorm(0.3, 0.1)

path.GA <- matrix(0, 2, 2)
path.GA[1, 1:2] <- NA
GA <- simMatrix(path.GA, "u35")

path.BE <- matrix(0, 2, 2)
path.BE[2, 1] <- NA
BE <- simMatrix(path.BE, "u57")

exo.cor <- matrix(NA, 2, 2)
diag(exo.cor) <- 1
RPH <- symMatrix(exo.cor, "n31")

RPS <- symMatrix(diag(2))

Path.Exo.Model <- simSetPath(RPS = RPS, BE = BE, RPH = RPH, GA = GA, exo=TRUE)
}
