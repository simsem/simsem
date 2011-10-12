\name{misspecified.Path.object}
\alias{misspecified.Path.object}
\title{
	Set of model misspecification for Path analysis model.
}
\description{
	This function will define model misspecifiction from a defined model. This function is similar to \code{\link{matrix.Path.object}} 
	such that the matrices that indicates misspecification will be added as arguments in the function. However, users do not have to add all matrices and vectors 
	in the function. Only element indicating misspecification is added.
}
\usage{
misspecified.Path.object(..., exo = FALSE)
}
\arguments{
  \item{\dots}{
	Arguments definition is listed in the \code{Details} section of \code{\link{matrix.Path.object}}. 
	Again, this function does not require to list all required matrices or vectors like the \code{\link{matrix.Path.object}} function.
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
		\item \code{\link{matrix.Path.object}} for matrix definition and how to specify Path analysis model
		\item \code{\linkS4class{simMisspecifiedSet}} for the result object
		\item \code{\link{misspecified.CFA.object}} for misspecification model in CFA and \code{\link{misspecified.SEM.object}} for misspecification model in SEM.
	}
}
\examples{
u1 <- runif.object(-0.1, 0.1)
mis.path.GA <- matrix(0, 2, 2)
mis.path.GA[2, 1:2] <- NA
mis.GA <- matrix.object(mis.path.GA, "u1")
Path.Mis.Model <- misspecified.Path.object(GA = mis.GA, exo=TRUE)
}
