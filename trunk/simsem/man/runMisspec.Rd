\name{runMisspec}
\alias{runMisspec}
\title{
	Draw actual parameters and model misspecification
}
\description{
	Randomly draw actual parameters from \code{\linkS4class{SimSet}} object and randomly draw the misspecified parameters from \code{\linkS4class{SimMisspec}} object. Then, the function will put the misspecified parameters on top of the actual parameters. 
}
\usage{
runMisspec(object, misspec, SimEqualCon=new("NullSimEqualCon"), conBeforeMis=TRUE)
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
  \item{conBeforeMis}{
	If \code{TRUE}, put model constraint on the actual parameters. If \code{FALSE}, put the equality constriant on the combined parameters (actual + misspecified).
}
}
\value{
	A list of with two elements: 1) \code{param} \code{\linkS4class{SimRSet}} of real model parameters and 2) \code{misspec} \code{\linkS4class{SimRSet}} of real model parameters with model misspecification
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\examples{
# No example
}
