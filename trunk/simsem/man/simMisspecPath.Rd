\name{simMisspecPath}
\alias{simMisspecPath}
\title{
	Set of model misspecification for Path analysis model.
}
\description{
	This function will define model misspecifiction from a defined model. This function is similar to \code{\link{simSetPath}} 
	such that the matrices that indicates misspecification will be added as arguments in the function. However, users do not have to add all matrices and vectors 
	in the function. Only element indicating misspecification is added.
}
\usage{
simMisspecPath(..., exo = FALSE)
}
\arguments{
  \item{\dots}{
	Arguments definition is listed in the \code{Details} section of \code{\link{simSetPath}}. 
	Again, this function does not require to list all required matrices or vectors like the \code{\link{simSetPath}} function.
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
		\item \code{\link{simSetPath}} for matrix definition and how to specify Path analysis model
		\item \code{\linkS4class{SimMisspec}} for the simResult
		\item \code{\link{simMisspecCFA}} for misspecification model in CFA and \code{\link{simMisspecSEM}} for misspecification model in SEM.
	}
}
\examples{
u1 <- simUnif(-0.1, 0.1)
mis.path.GA <- matrix(0, 2, 2)
mis.path.GA[2, 1:2] <- NA
mis.GA <- simMatrix(mis.path.GA, "u1")
Path.Mis.Model <- simMisspecPath(GA = mis.GA, exo=TRUE)
}
