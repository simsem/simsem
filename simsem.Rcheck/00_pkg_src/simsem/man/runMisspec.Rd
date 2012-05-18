\name{runMisspec}
\alias{runMisspec}
\title{
	Draw actual parameters and model misspecification
}
\description{
	Randomly draw actual parameters from \code{\linkS4class{SimSet}} object and randomly draw the misspecified parameters from \code{\linkS4class{SimMisspec}} object. Then, the function will put the misspecified parameters on top of the actual parameters. 
}
\usage{
runMisspec(object, misspec, SimEqualCon=new("NullSimEqualCon"))
}
\arguments{
  \item{object}{
	The \code{\linkS4class{SimSet}} object that saves the actual parameters
}
  \item{misspec}{
	The \code{\linkS4class{SimSet}} object that saves the misspecified parameters
}
  \item{SimEqualCon}{
	The equality constraint to equate parameters
}
}
\value{
	A list of with two elements: 1) \code{param} \code{\linkS4class{SimRSet}} of real model parameters, 2) \code{misspec} \code{\linkS4class{SimRSet}} of real model parameters with model misspecification, and 3) \code{misspecAdd} the misspecification alone
}
\details{
	When the equality constraint does not exist, there are two orders of generating parameters:
	\enumerate{
		\item \code{SimMisspec@misBeforeFill=TRUE} The misspecification will be added into user-specified parameters and the auto-completion on the other parameters.
		\item \code{SimMisspec@misBeforeFill=FALSE} The auto-completion will be applied first, then the misspecification will be added, and finally the auto-completion works again to adjust the parameter after adding misspecification.
	}
	When the equality constraint exists, there are six orders of generating parameters:
	\enumerate{
		\item \code{SimMisspec@misBeforeFill=TRUE & SimMisspec@conBeforeMis=TRUE & SimEqualCon@conBeforeFill=TRUE} The equality constraint is applied first, then the misspecification is added, and finally the auto-completion is applied.
		\item \code{SimMisspec@misBeforeFill=FALSE & SimMisspec@conBeforeMis=TRUE & SimEqualCon@conBeforeFill=TRUE} The equality constarint is applied first, then the auto-completion is applied, the misspecification will be added, and finally the auto-completion is used to adjusted for the added parameters.
		\item \code{SimMisspec@misBeforeFill=FALSE & SimMisspec@conBeforeMis=TRUE & SimEqualCon@conBeforeFill=FALSE} The auto-completeion is applied first, then the equality constriant is applied with adjusting parameters by the auto-completion, and finally the misspecification is added with adjusting the parameters by the auto-completion.
		\item \code{SimMisspec@misBeforeFill=FALSE & SimMisspec@conBeforeMis=FALSE & SimEqualCon@conBeforeFill=FALSE} The auto-completeion is applied first, then the misspecification is added with adjusting paramters by the auto-completion, and finally the equality constraint is added with adjusting parameters by the auto-completion.
		\item \code{SimMisspec@misBeforeFill=TRUE & SimMisspec@conBeforeMis=FALSE & SimEqualCon@conBeforeFill=TRUE} The misspecification is added first, then the equality constraint is applied, and finally the auto-completion is applied.
		\item \code{SimMisspec@misBeforeFill=TRUE & SimMisspec@conBeforeMis=FALSE & SimEqualCon@conBeforeFill=FALSE} The misspecification is added first, then the auto-completion is applied, and finally the equality constriant is applied with adjusting parameters by the auto-completion.
	}
	The default in this program is the first option for both scenarios. Other scenarios will be useful when users would like to put model misspecification or equality constraints on the parameters that users do not manually specify.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\examples{
# No example
}
