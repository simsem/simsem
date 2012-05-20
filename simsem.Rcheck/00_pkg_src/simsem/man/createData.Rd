\name{createData}
\alias{createData}
\title{
	Create data from model parameters
}
\description{
	Create data from model parameters. The data generation can be multivariate normal, copula model, or Bollen-Stine bootstrap model.
}
\usage{
createData(paramSet, n, object, dataOnly)
}
\arguments{
  \item{paramSet}{
	The list of parameters used for creating data. This list has one element as the actual parameters and the misspecification parameters.
}
  \item{n}{
	Sample size
}
  \item{object}{
	The data object, \code{\linkS4class{SimData}}
}
  \item{dataOnly}{
	\code{TRUE} to create data in \code{data.frame} only. \code{FALSE} to create data output object, \code{\linkS4class{SimDataOut}}
}
}
\value{
	A \code{data.frame} or a data output object, \code{\linkS4class{SimDataOut}}
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
# No example
}
