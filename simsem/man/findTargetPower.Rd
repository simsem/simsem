\name{findTargetPower}
\alias{findTargetPower}
\title{
	Find a value of varying parameters that provides a given value of power. 
}
\description{
	Find a value of varying parameters that provides a given value of power. This function can deal with only one varying parameter only (\code{\link{findPower}} can deal with more than one varying parameter).
}
\usage{
findTargetPower(iv, dv, power)
}
\arguments{
  \item{iv}{
	A vector of the target varying parameter
}
  \item{dv}{
	A \code{data.frame} of the power table of target parameters
}
  \item{power}{
	A desired power.
}
}
\value{
	The value of the target varying parameter providing the desired power. If the value is \code{NA}, there is no value in the domain of varying parameters that provide the target power. If the value is the minimum value of the varying parameters, it means that the minimum value has already provided enough power. The value of varying parameters that provides exact desired power may be lower than the minimum value.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\link{getPower}} to find the power of parameter estimates		
		\item \code{\link{continuousPower}} to find the power of parameter estimates for the result object (\code{linkS4class{SimResult}}) with varying parameters.
		\item \code{\link{findPower}} to find a value of varying parameters that provides a given value of power, which can deal with more than one varying parameter
	}
}
\examples{
# No example
}
