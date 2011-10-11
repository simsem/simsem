\name{misspecified.SEM.object}
\alias{misspecified.SEM.object}
\title{
	Set of model misspecification for SEM model.
}
\description{
	This function will define model misspecifiction from a defined model. This function is similar to \code{\link{matrix.SEM.object}} 
	such that the matrices that indicates misspecification will be added as arguments in the function. However, users do not have to add all matrices and vectors 
	in the function. Only element indicating misspecification is added.
}
\usage{
misspecified.SEM.object(..., exo = FALSE)
}
\arguments{
  \item{\dots}{
	Arguments definition is listed in the \code{Details} section of \code{\link{matrix.SEM.object}}. 
	Again, this function does not require to list all required matrices or vectors like the \code{\link{matrix.SEM.object}} function.
	Only misspecification is added.
}
  \item{exo}{
	specify \code{TRUE} if users wish to specify both exogenous and endogenous indicators.
}
}
\value{
	object in \code{\linkS4class{simMisspecifiedSet}} that saves model misspecification.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\enumerate{
		\item \code{\link{matrix.SEM.object}} for matrix definition and how to specify SEM model
		\item \code{\linkS4class{simMisspecifiedSet}} for the result object
		\item \code{\link{misspecified.CFA.object}} for misspecification model in CFA and \code{\link{misspecified.Path.object}} for misspecification model in Path analysis.
	}
}
\examples{
u2 <- runif.object(-0.2, 0.2)
n1 <- rnorm.object(0, 0.1)
loading.X.trivial <- matrix(NA, 6, 2)
loading.X.trivial[is.na(loading.X.trivial)] <- 0
LX.trivial <- matrix.object(loading.X.trivial, "u2")
error.cor.X.trivial <- matrix(NA, 6, 6)
diag(error.cor.X.trivial) <- 0
TD.trivial <- sym.matrix.object(error.cor.X.trivial, "n1")
error.cor.Y.trivial <- matrix(NA, 2, 2)
diag(error.cor.Y.trivial) <- 0
TE.trivial <- sym.matrix.object(error.cor.Y.trivial, "n1")
TH.trivial <- matrix.object(matrix(NA, 6, 2), "n1")
SEM.Mis.Model <- misspecified.SEM.object(LX = LX.trivial, TE = TE.trivial, TD = TD.trivial, TH = TH.trivial, exo=TRUE)
}
