\name{combineLatentCorExoEndo}
\alias{combineLatentCorExoEndo}
\title{
	Combine exogenous factor correlation and endogenous factor correlation into a single matrix
}
\description{
	Combine exogenous factor correlation, \code{PH}, and endogenous factor correlation, \code{PS}, into a single matrix
}
\usage{
combineLatentCorExoEndo(PH, PS, value=0)
}
\arguments{
  \item{PH}{
	The exogenous factor correlation matrix
}
  \item{PS}{
	The endogenous factor correlation matrix
}
  \item{value}{
	The values for the correlation between exogenous and endogenous variables
}
}
\value{
	The combined correlation matrix
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
# No example
}
