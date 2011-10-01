\name{model.object}
\alias{model.object}
\title{
	Create model object from model specification
}
\description{
	This function will take model specification from \code{simMatrixSet} that contains free parameters, starting values, and fixed values. It will transform the code to a specified SEM package and ready to analyze data.
}
\usage{
model.object(object, ...)
}
\arguments{
  \item{object}{
	\code{\linkS4class{simMatrixSet}} or \code{freeParamSet} that provides model specification
}
  \item{\dots}{
	Other values that will be explained specifically for each class
}
}
\value{
	\code{simModel} that will be used for data analysis
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\seealso{
	Each method link
}
\examples{
#loading <- matrix(0, 6, 2)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loadingValues <- matrix(0, 6, 2)
#loadingValues[1:3, 1] <- 0.7
#loadingValues[4:6, 2] <- 0.7
#LX <- matrix.object(loading, loadingValues)
#summary(LX)

#latent.cor <- matrix(NA, 2, 2)
#diag(latent.cor) <- 1
#PH <- sym.matrix.object(latent.cor, 0.5)

#error.cor <- matrix(0, 6, 6)
#diag(error.cor) <- 1
#TD <- sym.matrix.object(error.cor)

#CFA.Model <- matrix.CFA.object(LX = LX, PH = PH, TD = TD)

#SimModel <- model.object(CFA.Model)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
