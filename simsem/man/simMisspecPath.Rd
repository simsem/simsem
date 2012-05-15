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
simMisspecPath(..., exo = FALSE, conBeforeMis=TRUE, misBeforeFill=TRUE, 
misfitType="rmsea", misfitBound=new("NullVector"), averageNumMisspec=FALSE, 
optMisfit="none", numIter=20)
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
  \item{conBeforeMis}{
	\code{TRUE} if users wish to constrain parameters before adding misspecification. \code{FALSE} if users wish to constrain parameters after adding misspecification.
}
  \item{misBeforeFill}{
	\code{TRUE} if users wish to apply misspecification before applying the auto-completion on the parameters that users have not specified. \code{FALSE} if users wish to apply the auto-completion before adding misspecification. This option is helpful when users wish to apply misspecification on the parameters that users have not specified (e.g., adding trivial misspecification on the residual variance, which users let the package to calculate it and not specify it). See \code{\link{runMisspec}} for further details.
}
  \item{misfitType}{
	The type of population misfit used in the \code{misfitBound} below. The default is \code{"rmsea"}. The two other options are \code{"f0"} and \code{"srmr"}. See \code{\link{popMisfitMACS}} for further details.
}
  \item{misfitBound}{
	The lower and upper bounds of the population misfit. This option must be a vector with two elements. 
}
  \item{averageNumMisspec}{
	If \code{TRUE}, the misfit will be divided by the number of free elements in the misspecification object. The default is \code{FALSE}.
}
  \item{optMisfit}{
	Use the optimization method to pick the misspecification set. That is, the program will draw a number of misspecification sets. Then, the different sets of misspecification will be compared together. If \code{"min"} is specified, the program will pick the misspecification set the provides the least amount of misfit. If \code{"max"} is specified, the program will pick the set that has the largets misfit. The default is \code{"none"} to not use the optimization method.
}
  \item{numIter}{
	The number of different misspecification sets for comparison in the optimization method.
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
