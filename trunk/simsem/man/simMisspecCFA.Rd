\name{simMisspecCFA}
\alias{simMisspecCFA}
\title{
	Set of model misspecification for CFA model.
}
\description{
	This function will define model misspecifiction from a defined model. This function is similar to \code{\link{simSetCFA}} 
	such that the matrices that indicates misspecification will be added as arguments in the function. However, users do not have to add all matrices and vectors 
	in the function. Only element indicating misspecification is added.
}
\usage{
simMisspecCFA(...)
}
\arguments{
  \item{\dots}{
	Arguments definition is listed in the \code{Details} section of \code{\link{simSetCFA}}. Again, this function does not require to list all required matrices or vectors like the \code{\link{simSetCFA}} function.
	Only misspecification is added.
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
		\item \code{\link{simSetCFA}} for matrix definition and how to specify CFA model
		\item \code{\linkS4class{SimMisspec}} for the simResult
		\item \code{\link{simMisspecPath}} for misspecification model in Path analysis and \code{\link{simMisspecSEM}} for misspecification model in SEM.
	}
}
\examples{
n01 <- simNorm(0, 0.1)
error.cor.Mis <- matrix(NA, 6, 6)
diag(error.cor.Mis) <- 1
TD.Mis <- symMatrix(error.cor.Mis, "n01")
CFA.Model.Mis <- simMisspecCFA(TD=TD.Mis)
}
