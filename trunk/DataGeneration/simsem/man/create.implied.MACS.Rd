\name{create.implied.MACS}
\alias{create.implied.MACS}
\title{
	Create model implied Means and Covariance Matrix (MACS) 
}
\description{
	Create model implied means and covariance matrix from a parameter set from any SEM model.
}
\usage{
create.implied.MACS(object)
}
\arguments{
  \item{object}{
	\code{matrixSet} (both X and Y sides) or \code{reducedMatrixSet} (Y side only) that contains model parameters
}
}
\details{
	This function create model implied mean and covariance matrix by formulas.
}
\value{
	\item{M}{Model implied mean}
	\item{CM}{Model implied covariance matrix}
}
\references{
Ansari, A., Jedidi, K., & Jagpal, S. (2000). A hierarchical Bayesian methodology for treating heterogeneity in structural equation models. Marketing Science, 328-347.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\note{
	The equation is ... (TBA).
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
#CFA.Model.Param <- run(CFA.Model)
#create.implied.MACS(CFA.Model.Param)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
