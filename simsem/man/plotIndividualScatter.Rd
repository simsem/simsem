\name{plotIndividualScatter}
\alias{plotIndividualScatter}
\title{
Plot an overlaying scatter plot visualizing the power of rejecting misspecified models
}
\description{
Plot the fit indices value against the value of predictors. The plot will include the fit indices value of the alternative models, the fit indices value of the null model (if specified), and the fit indices cutoffs (if specified).  
}
\usage{
plotIndividualScatter(altVec, nullVec=NULL, cutoff=NULL, x, main = NULL)
}
\arguments{
  \item{altVec}{
	The vector saving the fit index distribution when the hypothesized model is \code{FALSE}.
}
  \item{nullVec}{
	The vector saving the fit index distribution when the hypothesized model is \code{TRUE}.
}
  \item{cutoff}{
	A priori cutoff 
}
\item{x}{
	The \code{data.frame} of the predictor values. The number of rows of the \code{x} argument should be equal to the number of rows in the \code{object}.
}
  \item{main}{
	The title of the graph
}
}
\value{
	NONE. Only plot the fit indices distributions.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\link{plotPowerFit}} 
	}
}
\examples{
# No example
}
