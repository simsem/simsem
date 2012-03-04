\name{simMisspecSEM}
\alias{simMisspecSEM}
\title{
	Set of model misspecification for SEM model.
}
\description{
	This function will define model misspecifiction from a defined model. This function is similar to \code{\link{simSetSEM}} 
	such that the matrices that indicates misspecification will be added as arguments in the function. However, users do not have to add all matrices and vectors 
	in the function. Only element indicating misspecification is added.
}
\usage{
simMisspecSEM(..., exo = FALSE)
}
\arguments{
  \item{\dots}{
	Arguments definition is listed in the \code{Details} section of \code{\link{simSetSEM}}. 
	Again, this function does not require to list all required matrices or vectors like the \code{\link{simSetSEM}} function.
	Only misspecification is added.
}
  \item{exo}{
	specify \code{TRUE} if users wish to specify both exogenous and endogenous indicators.
}
}
\value{
	object in \code{\linkS4class{SimMisspec}} that saves model misspecification.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\link{simSetSEM}} for matrix definition and how to specify SEM model
		\item \code{\linkS4class{SimMisspec}} for the simResult
		\item \code{\link{simMisspecCFA}} for misspecification model in CFA and \code{\link{simMisspecPath}} for misspecification model in Path analysis.
	}
}
\examples{
u2 <- simUnif(-0.2, 0.2)
n1 <- simNorm(0, 0.1)
loading.X.trivial <- matrix(NA, 6, 2)
loading.X.trivial[is.na(loading.X.trivial)] <- 0
LX.trivial <- simMatrix(loading.X.trivial, "u2")
error.cor.X.trivial <- matrix(NA, 6, 6)
diag(error.cor.X.trivial) <- 0
RTD.trivial <- symMatrix(error.cor.X.trivial, "n1")
error.cor.Y.trivial <- matrix(NA, 2, 2)
diag(error.cor.Y.trivial) <- 0
RTE.trivial <- symMatrix(error.cor.Y.trivial, "n1")
RTH.trivial <- simMatrix(matrix(NA, 6, 2), "n1")
SEM.Mis.Model <- simMisspecSEM(LX = LX.trivial, RTE = RTE.trivial, RTD = RTD.trivial, RTH = RTH.trivial, exo=TRUE)
}
