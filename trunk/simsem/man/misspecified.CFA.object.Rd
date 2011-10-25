\name{misspecified.CFA.object}
\alias{misspecified.CFA.object}
\title{
	Set of model misspecification for CFA model.
}
\description{
	This function will define model misspecifiction from a defined model. This function is similar to \code{\link{matrix.CFA.object}} 
	such that the matrices that indicates misspecification will be added as arguments in the function. However, users do not have to add all matrices and vectors 
	in the function. Only element indicating misspecification is added.
}
\usage{
misspecified.CFA.object(...)
}
\arguments{
  \item{\dots}{
	Arguments definition is listed in the \code{Details} section of \code{\link{matrix.CFA.object}}. Again, this function does not require to list all required matrices or vectors like the \code{\link{matrix.CFA.object}} function.
	Only misspecification is added.
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
		\item \code{\link{matrix.CFA.object}} for matrix definition and how to specify CFA model
		\item \code{\linkS4class{simMisspecifiedSet}} for the result object
		\item \code{\link{misspecified.Path.object}} for misspecification model in Path analysis and \code{\link{misspecified.SEM.object}} for misspecification model in SEM.
	}
}
\examples{
n01 <- rnorm.object(0, 0.1)
error.cor.Mis <- matrix(NA, 6, 6)
diag(error.cor.Mis) <- 1
TD.Mis <- sym.matrix.object(error.cor.Mis, "n01")
CFA.Model.Mis <- misspecified.CFA.object(TD=TD.Mis)
}
