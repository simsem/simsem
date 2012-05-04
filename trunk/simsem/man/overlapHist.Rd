\name{overlapHist}
\alias{overlapHist}
\title{
	Plot overlapping histograms
}
\description{
	Plot overlapping histograms
}
\usage{
overlapHist(a, b, colors=c("red","blue","purple"), breaks=NULL, xlim=NULL, 
	ylim=NULL, main=NULL, xlab=NULL, swap=FALSE)
}
\arguments{
  \item{a}{
	Data for the first histogram
}
  \item{b}{
	Data for the second histogram
}
  \item{colors}{
	Colors for the first histogram, the second histogram, and the overlappling areas.
}
  \item{breaks}{
	How many breaks users used in each histogram (should not be used)
}
  \item{xlim}{
	The range of x-axis
}
  \item{ylim}{
	The range of y-axis
}
  \item{main}{
	The title of the figure
}
  \item{xlab}{
	The labels of x-axis
}
  \item{swap}{
	Specify \code{TRUE} to plot \code{b} first and then \code{a}. The default is \code{FALSE} to plot \code{a} first and then \code{b}.
}
}
\value{
	None. This function will plot only.
}
\author{
    Chris Miller provided this code on \url{http://chrisamiller.com/science/2010/07/20/transparent-overlapping-histograms-in-r/}. The code is modified by Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\examples{
# This function is not a public function.

# a <- rnorm(10000, 0, 1)
# b <- rnorm(10000, 1, 1.5)
# overlapHist(a, b, main="Example")
}
