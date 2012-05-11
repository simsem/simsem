\name{drawParametersMisspec}
\alias{drawParametersMisspec}
\title{
	Create parameter sets (with or without model misspecification) from the parameter with or without misspecification set
}
\description{
	Create parameter sets (with or without model misspecification) from the parameter (\code{\linkS4class{SimSet}}) with or without misspecification set (\code{\linkS4class{SimMisspec}})
}
\usage{
drawParametersMisspec(objSet, objMisspec = new("NullSimMisspec"), objEqualCon = new("NullSimEqualCon"), maxDraw=100)
}
\arguments{
  \item{objSet}{
	The parameter set object, \code{\linkS4class{SimSet}}, that saves the specification of the actual parameter
}
  \item{objMisspec}{
	The misspecified parameter set object, \code{\linkS4class{SimMisspec}}, that saves the specification of the misspecified parameter
}
  \item{objEqualCon}{
	The equality constraint object, \code{\linkS4class{SimEqualCon}}
}
  \item{maxDraw}{
	The maximum number of sample drawn. This function will draw sets of actual and misspecified parameters repeatedly until the identified sets of actual and misspecified parameters are drawn. The maximum of repetition is specified by this argument.
}
}
\value{
	A list of with two elements: 1) \code{param} \code{\linkS4class{SimRSet}} of real model parameters, 2) \code{misspec} \code{\linkS4class{SimRSet}} of real model parameters with model misspecification, and 3) \code{misspecAdd} the misspecification alone
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
# No example
}
