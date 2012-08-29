\name{simDataDist}
\alias{simDataDist}
\title{
	Create a data distribution object.
}
\description{
TBA
}
\usage{
simDataDist(..., p=NULL, keepScale=TRUE, reverse=FALSE)
}
\arguments{
  \item{...}{
	List of distribution objects. See \code{\linkS4class{VirtualDist}} for a list of possible distributions. 
}
  \item{p}{
	Number of variables. If only one distribution object is listed, the \code{p} will make the same distribution objects for all variables.
}
  \item{keepScale}{
	A vector representing whether each variable is transformed its mean and standard deviation or not. If TRUE, transform back to retain the mean and standard deviation of a variable equal to the model implied mean and standard deviation (with sampling error)
}
  \item{reverse}{
	A vector representing whether each variable is mirrored or not. If \code{TRUE}, reverse the distribution of a variable (e.g., from positive skewed to negative skewed. If one logical value is specified, it will apply to all variables.
}
}
\value{
	\code{\linkS4class{SimDataDist}} that saves analysis result from simulate data.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimResult}} for the type of resulting object
	}
}
\examples{
# Need an example
}
