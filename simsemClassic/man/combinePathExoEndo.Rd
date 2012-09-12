\name{combinePathExoEndo}
\alias{combinePathExoEndo}
\title{
	Combine the regression coefficient matrices
}
\description{
	Combine the regression coefficient matrices both one from exogenous variables to endogenous variables and one from endogenous variables to endogenous variables
}
\usage{
combinePathExoEndo(GA, BE, value = 0)
}
\arguments{
  \item{GA}{
	The regression coefficient matrix from exogenous variables to endogenous variables
}
  \item{BE}{
	The regression coefficient matrix from endogenous variables to endogenous variables
}
  \item{value}{
	The value put in the leftovers in the resulting object (such as exogenous variables --> exogenous variables)
}
}
\value{
	The combined regression coefficient matrix
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
# No example
}
