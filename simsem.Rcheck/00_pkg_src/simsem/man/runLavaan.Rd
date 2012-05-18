\name{runLavaan}
\alias{runLavaan}
\title{
	Run data by the model object by the \code{lavaan} package
}
\description{
	Transform model object (\code{\linkS4class{SimModel}}) to \code{lavaan} script, run the obtained data, and make model output object (\code{\linkS4class{SimModelOut}})
}
\usage{
runLavaan(object, Data, miss="fiml", estimator="ML")
}
\arguments{
  \item{object}{
	The model object (\code{\linkS4class{SimModel}})
}
  \item{Data}{
	The data to be analyzed
}
  \item{miss}{
	The method to handle missing data in lavaan. The default is full information maximum likelihood.
}
  \item{estimator}{
	The method of estimation. The default is maximum likelihood (ML).
}
}
\value{
	The model output object that saves parameter estimates, standard errors, and fit indices.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\examples{
# No example
}
