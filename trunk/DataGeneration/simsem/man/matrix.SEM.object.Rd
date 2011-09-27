\name{matrix.SEM.object}
\alias{matrix.SEM.object}
\title{
	Create a set of matrix belongs to SEM model
}
\description{
	This function will create set of matrix that belongs to full SEM model. The requirement is to specify factor residual correlation matrix, regression coefficient matrix, factor loading matrix, and measurement error correlation.	
}
\usage{
matrix.SEM.object(..., exo = FALSE)
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
	REQUIRED: \code{LY} for factor loading matrix from endogenous factors to Y indicators (need to be \code{simMatrix} object). 
	REQUIRED: \code{TE} for measurement error correlation matrix among Y indicators (need to be \code{symMatrix} object).
	REQUIRED: \code{BE} for regression coefficient matrix among endogenous factors (need to be \code{simMatrix} object). 
	REQUIRED: \code{PS} for residual correlation matrix among endogenous factors (need to be \code{symMatrix} object).
	\code{VTE} for measurement error variance of Y indicators (need to be \code{simVector} object).
	\code{VY} for total variance of Y indicators (need to be \code{simVector} object).
	NOTE: Either measurement error variance or indicator variance is specified. Both cannot be simultaneously specified.
	\code{TY} for measurement intercepts of Y indicators. (need to be \code{simVector} object).
	\code{MY} for overall Y indicator means. (need to be \code{simVector} object).
	NOTE: Either measurement intercept of indicator mean can be specified. Both cannot be specified simultaneously.
	\code{VPS} for residual variance of endogenous factors (need to be \code{simVector} object).
	\code{VE} for total endogenous factor variance (need to be \code{simVector} object).
	NOTE: Either total endogenous factor variance or residual endogenous factor variance is specified. Both cannot be simultaneously specified.
	\code{AL} for endogenous factor intercept (need to be \code{simVector} object).
	\code{ME} for total mean of endogenous factors (need to be \code{simVector} object).
	NOTE: Either endogenous factor intercept or total mean of endogenous factor is specified. Both cannot be simultaneously specified.

	NOTE: If users need to specify exogenous variable too.
	REQUIRED for \code{"exo=TRUE"}: \code{LX} for factor loading matrix from exogenous factors to X indicators (need to be \code{simMatrix} object). 
	REQUIRED for \code{"exo=TRUE"}: \code{TD} for measurement error correlation matrix among X indicators (need to be \code{symMatrix} object).
	REQUIRED for \code{"exo=TRUE"}: \code{GA} for regression coefficient matrix among exogenous factors (need to be \code{simMatrix} object). 
	REQUIRED for \code{"exo=TRUE"}: \code{PH} for residual correlation matrix among exogenous factors (need to be \code{symMatrix} object).
	\code{VTD} for measurement error variance of X indicators (need to be \code{simVector} object).
	\code{VX} for total variance of X indicators (need to be \code{simVector} object).
	NOTE: Either measurement error variance or indicator variance is specified. Both cannot be simultaneously specified.
	\code{TX} for measurement intercepts of Y indicators. (need to be \code{simVector} object).
	\code{MX} for overall Y indicator means. (need to be \code{simVector} object).
	NOTE: Either measurement intercept of indicator mean can be specified. Both cannot be specified simultaneously.
	\code{VPH} or \code{VK} for total exogenous factor variance (need to be \code{simVector} object).
	\code{KA} or \code{MK} for total mean of exogenous factors (need to be \code{simVector} object).

	DEFAULT: 1) All indicator variances are equal to 1. Measurement error variances are automatically implied from total indicator variances.
		2) All measurement error variances are free parameters.
		3) All indicator means are equal to 0. Indicator intercepts are automatically implied from indicator means.
		4) All indicator intercepts are free parameters.
		5) All factor variances are equal to 1.
		6) All factor variances are fixed.
		7) All factor means are equal to 0.
		8) All factor means are fixed.
}
\value{
	\code{simMatrixSet} object that represents the SEM object. This will be used for specifying data or model objects later.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\seealso{
	See class \code{\linkS4class{simMatrixSet}} for result object details. See \code{\linkS4class{simMatrix}}, \code{\linkS4class{symMatrix}}, or \code{\linkS4class{simVector}} for input details.
	Use \code{\link{matrix.CFA.object}} to specify CFA model and use \code{\link{matrix.Path.object}} to specify path analysis model.
}
\examples{
loading <- matrix(0, 8, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:8, 3] <- NA
loading.start <- matrix("", 8, 3)
loading.start[1:3, 1] <- 0.7
loading.start[4:6, 2] <- 0.7
loading.start[7:8, 3] <- "u68"
LY <- matrix.object(loading, loading.start)

TE <- sym.matrix.object(diag(8))

factor.cor <- diag(3)
factor.cor[1, 2] <- factor.cor[2, 1] <- NA
PS <- sym.matrix.object(factor.cor, 0.5)

path <- matrix(0, 3, 3)
path[3, 1:2] <- NA
path.start <- matrix(0, 3, 3)
path.start[3, 1] <- "n65"
path.start[3, 2] <- "u35"
BE <- matrix.object(path, path.start)

SEM.model <- matrix.SEM.object(BE=BE, LY=LY, PS=PS, TE=TE)

loading.X <- matrix(0, 6, 2)
loading.X[1:3, 1] <- NA
loading.X[4:6, 2] <- NA
LX <- matrix.object(loading.X, 0.7)

loading.Y <- matrix(NA, 2, 1)
LY <- matrix.object(loading.Y, "u68")

TD <- sym.matrix.object(diag(6))

TE <- sym.matrix.object(diag(2))

factor.K.cor <- matrix(NA, 2, 2)
diag(factor.K.cor) <- 1
PH <- sym.matrix.object(factor.K.cor, 0.5)

PS <- sym.matrix.object(as.matrix(1))

path.GA <- matrix(NA, 1, 2)
path.GA.start <- matrix(c("n65", "u35"), ncol=2)
GA <- matrix.object(path.GA, path.GA.start)

BE <- matrix.object(as.matrix(0))

SEM.Exo.model <- matrix.SEM.object(GA=GA, BE=BE, LX=LX, LY=LY, PH=PH, PS=PS, TD=TD, TE=TE, exo=TRUE)
}
