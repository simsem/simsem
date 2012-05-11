\name{plot3DQtile}
\alias{plot3DQtile}
\title{
	Build a persepctive plot or contour plot of a quantile of predicted values
}
\description{
	Build a persepctive plot or contour plot of a quantile of predicted values
}
\usage{
plot3DQtile(x, y, z, df=0, qtile=0.5, useContour=TRUE, xlab=NULL, 
	ylab=NULL, zlab=NULL, main=NULL)
}
\arguments{
  \item{x}{
	The values of the first variable (e.g., a vector of sample size)
}
  \item{y}{
	The values of the second variable (e.g., a vector of percent missing)
}
  \item{z}{
	The values of the dependent variable
}
  \item{df}{
	The degree of freedom in spline method
}
  \item{qtile}{
	The quantile values used to plot a graph
}
  \item{useContour}{
	If \code{TRUE}, use contour plot. If \code{FALSE}, use perspective plot.
}
  \item{xlab}{
	The labels of x-axis
}
  \item{ylab}{
	The labels of y-axis
}
  \item{zlab}{
	The labels of z-axis
}
  \item{main}{
	The title of the graph
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
