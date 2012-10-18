\name{plotLogitMiss}
\alias{plotLogitMiss}
\title{
	Visualize the missing proportion when the logistic regression method is used.
}
\description{
	Visualize the missing proportion when the logistic regression method is used. The maximum number of independent variables is 2. The extra independent variables will be fixed as a value (the default is 0).
}
\usage{
plotLogitMiss(script, ylim=c(0,1), x1lim=c(-3,3), x2lim=c(-3,3), otherx=0, 
	useContour=TRUE)
}
\arguments{
  \item{script}{
	The script used in specifying missing data using the logistic regression. See further details in the \code{logit} argument of the \code{\link{miss}} function
}
  \item{ylim}{
	The range of missing proportion to be plotted.
}
  \item{x1lim}{
	The range of the first independent variable to be plotted
}
\item{x2lim}{
	The range of the second independent variable to be plotted
}
\item{otherx}{
	The value of the extra independent variables to be fixed as.
}
\item{useContour}{
	If there are two or more independent variables, the function will provide 3D graph. Contour graph is a default. However, if this is specified as \code{FALSE}, perspective plot is used.
}
}
\value{
	Not return any value. This function will plot a graph only. If the number of independent variable is 0, the bar graph is provided. If the number of independent variables is 1, the logistic curve is provided. If the number of independent variables is 2, contour or perspective plot is provided.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu}), Alexander M. Schoemann (University of Kansas; \email{schoemann@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\link{miss}} to create the missing data template
		\item \code{\link{impose}} to impose missing data
	}
}
\examples{
script <- 'y1 ~ 0.05 + 0.1*y2 + 0.3*y3
	y4 ~ -2 + 0.1*y4
	y5 ~ -0.5' 
plotLogitMiss(script)

script2 <- 'y1 ~ 0.05 + 0.5*y3
	y2 ~ p(0.2)
	y3 ~ p(0.1) + -1*y1
	y4 ~ p(0.3) + 0.2*y1 + -0.3*y2
	y5 ~ -0.5' 
plotLogitMiss(script2)
}
