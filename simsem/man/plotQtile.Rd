\name{plotQtile}
\alias{plotQtile}
\title{
	Build a scatterplot with overlaying line of quantiles of predicted values
}
\description{
	Build a scatterplot with overlaying line of quantiles of predicted values
}
\usage{
plotQtile(x, y, df=0, qtile=0.5, ...)
}
\arguments{
  \item{x}{
	The values of the independent variable (e.g., a vector of sample size)
}
  \item{y}{
	The values of the dependent variable
}
  \item{df}{
	The degree of freedom in spline method
}
  \item{qtile}{
	The quantile values used to plot a graph
}
  \item{\dots}{
	Other arguments in the \code{plot} command
}
}
\value{
	None. This function will plot only.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\examples{
# No example
}
