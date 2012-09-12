\name{setPopulation}
\alias{setPopulation}
\alias{setPopulation-methods}
\alias{setPopulation,ANY-method}
\title{
	Set the data generation population model underlying an object
}
\description{
This function will set the data generation population model to be an appropriate one. If the appropriate data generation model is put (the same model as the analysis model), the additional features can be seen when we run a \code{\link{summary}} function on the target object, such as bias in parameter estimates or percentage coverage. 
}
\usage{
setPopulation(target, population, ...)
}
\arguments{
  \item{target}{
	The target object that you wish to set the data generation population model. The \code{target} argument can be \code{linkS4class{SimModelOut}} or \code{linkS4class{SimResult}}.
}
  \item{population}{
	The population parameters used to put
}
  \item{\dots}{
	An additional argument, such as, when values are saved in a set of matrices, a set of matrices that indicates the position of free parameters is needed in the \dots.
}
}
\value{
	The target object that is changed the parameter.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimModelOut}} for model output object
		\item \code{\linkS4class{SimResult}} for result object
	}
}
\examples{
# See each class for an example.
}
