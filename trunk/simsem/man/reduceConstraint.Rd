\name{reduceConstraint}
\alias{reduceConstraint}
\title{
	Reduce the model constraint to data generation parameterization to analysis model parameterization.
}
\description{
	Reduce the model constraint to data generation parameterization (in \code{\linkS4class{SimSet}}) to analysis model parameterization (in \code{linkS4class{SimRSet}}). Some symbols will be reduced to appropriate position such as \code{VTE[3]} to \code{TE[3, 3]}
}
\usage{
reduceConstraint(SimEqualCon)
}
\arguments{
  \item{SimEqualCon}{
	The equality constraint object, \code{\linkS4class{SimEqualCon}}
}
}
\value{
	The reduced equality constraint object, \code{\linkS4class{SimREqualCon}}
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\examples{
# No example
}
