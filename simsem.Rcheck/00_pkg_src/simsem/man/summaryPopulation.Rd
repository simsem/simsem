\name{summaryPopulation}
\alias{summaryPopulation}
\alias{summaryPopulation-methods}
\alias{summaryPopulation,ANY-method}
\title{
	Summarize the data generation population model underlying an object
}
\description{
This function will summarize the data generation population model underlying an object. The target object can be \code{linkS4class{SimModelOut}}, \code{linkS4class{SimDataOut}}, or \code{linkS4class{SimResult}}.
}
\usage{
summaryPopulation(object)
}
\arguments{
  \item{object}{
	The target object that you wish to extract the data generation population model from, which can be \code{linkS4class{SimModelOut}}, \code{linkS4class{SimDataOut}}, or \code{linkS4class{SimResult}}.
}
}
\value{
	None except using for \code{linkS4class{SimResult}} which the return value is a \code{data.frame} of the summary of population model across replications.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimDataOut}} for data output object
		\item \code{\linkS4class{SimModelOut}} for model output object
		\item \code{\linkS4class{SimResult}} for result object
	}
}
\examples{
# See each class for an example.
}
