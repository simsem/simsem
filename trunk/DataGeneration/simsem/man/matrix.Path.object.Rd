\name{matrix.Path.object}
\alias{matrix.Path.object}
\title{
	Create a set of matrix belongs to Path analysis model
}
\description{
	This function will create set of matrix that belongs to path analysis model. The requirement is to specify indicator correlation and regression coefficient matrix.	
}
\usage{
matrix.Path.object(..., exo = FALSE)
}
\arguments{
  \item{\dots}{
	Each element of model specification, as described in \code{Details}
}
  \item{exo}{
	specify TRUE if users wish to specify both exogenous and endogenous indicators.
}
}
\details{
	REQUIRED: \code{BE} for regression coefficient matrix (need to be \code{simMatrix} object). 
	REQUIRED: \code{PS} for residual correlation matrix (need to be \code{symMatrix} object).
	\code{VPS} for residual indicator variance (need to be \code{simVector} object).
	\code{VE} for total indicator variance (need to be \code{simVector} object).
	NOTE: Either total indicator variance or residual indicator variance is specified. Both cannot be simultaneously specified.
	\code{AL} for indicator intercept (need to be \code{simVector} object).
	\code{ME} for indicator total mean (need to be \code{simVector} object).
	NOTE: Either indicator intercept or indicator total mean is specified. Both cannot be simultaneously specified.


	\code{VPS} for residual indicator variance (need to be \code{simVector} object).
	\code{VE} for total indicator variance (need to be \code{simVector} object).
	NOTE: Either total indicator variance or residual indicator variance is specified. Both cannot be simultaneously specified.
	\code{AL} for indicator intercept (need to be \code{simVector} object).
	\code{ME} for indicator total mean (need to be \code{simVector} object).
	NOTE: Either indicator intercept or indicator total mean is specified. Both cannot be simultaneously specified.

	NOTE: If users need to specify exogenous variable too.
	REQUIRED for \code{"exo=TRUE"}: \code{GA} for regression coefficient matrix from exogenous variable to endogenous variable (need to be \code{simMatrix} object).  
	REQUIRED for \code{"exo=TRUE"}: \code{PH} for exogenous factor correlation (need to be \code{symMatrix} object).
	\code{VPH} or \code{VK} for exogenous variable variance (need to be \code{simVector} object).
	\code{KA} or \code{MK} for exogenous variable mean (need to be \code{simVector} object).

	DEFAULT: 1) All indicator variances are equal to 1. Residual variances are automatically implied from total indicator variances.
		2) All residual variances are free parameters.
		3) All indicator means are equal to 0. Intercepts are automatically implied from total indicator mean.
		4) All indicator intercepts are free parameters.
}
\value{
	\code{simMatrixSet} object that represents the path analysis model object. This will be used for specifying data or model objects later.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\seealso{
	See class \code{\linkS4class{simMatrixSet}} for result object details. See \code{\linkS4class{simMatrix}}, \code{\linkS4class{symMatrix}}, or \code{\linkS4class{simVector}} for input details.
	Use \code{\link{matrix.CFA.object}} to specify CFA model and use \code{\link{matrix.SEM.object}} to specify full structural equation modeling.
}
\examples{ 
u35 <- runif.object(0.3, 0.5)
u57 <- runif.object(0.5, 0.7)
u1 <- runif.object(-0.1, 0.1)
n31 <- rnorm.object(0.3, 0.1)

path.BE <- matrix(0, 4, 4)
path.BE[3, 1:2] <- NA
path.BE[4, 3] <- NA
starting.BE <- matrix("", 4, 4)
starting.BE[3, 1:2] <- "u35"
starting.BE[4, 3] <- "u57"
BE <- matrix.object(path.BE, starting.BE)

residual.error <- diag(4)
residual.error[1,2] <- residual.error[2,1] <- NA
PS <- sym.matrix.object(residual.error, "n31")

Path.Model <- matrix.Path.object(PS = PS, BE = BE)

u35 <- runif.object(0.3, 0.5)
u57 <- runif.object(0.5, 0.7)
u1 <- runif.object(-0.1, 0.1)
n31 <- rnorm.object(0.3, 0.1)

path.GA <- matrix(0, 2, 2)
path.GA[1, 1:2] <- NA
GA <- matrix.object(path.GA, "u35")

path.BE <- matrix(0, 2, 2)
path.BE[2, 1] <- NA
BE <- matrix.object(path.BE, "u57")

exo.cor <- matrix(NA, 2, 2)
diag(exo.cor) <- 1
PH <- sym.matrix.object(exo.cor, "n31")

PS <- sym.matrix.object(diag(2))

Path.Exo.Model <- matrix.Path.object(PS = PS, BE = BE, PH = PH, GA = GA, exo=TRUE)
}
