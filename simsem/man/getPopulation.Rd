\name{getPopulation}
\alias{getPopulation}
\alias{getPopulation-methods}
\alias{getPopulation,ANY-method}
\title{
	Extract the data generation population model underlying an object
}
\description{
This function will extract the data generation population model underlying an object. The target object can be \code{linkS4class{SimResult}}.
}
\usage{
getPopulation(object, ...)
}
\arguments{
  \item{object}{
	The target object that you wish to extract the data generation population model from, which can be \code{linkS4class{SimResult}}.
}
  \item{\dots}{
	An additional argument. The details can be seen when this funciton is applied to the \code{linkS4class{SimDataOut}}.
}
}
\value{
	Depends on the class of \code{object}.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimResult}} for result object
	}
}
\examples{
# See each class for an example.
}
