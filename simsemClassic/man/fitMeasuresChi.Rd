\name{fitMeasuresChi}
\alias{fitMeasuresChi}
\title{
	Find fit indices from the discrepancy values of the target model and null models. 
}
\description{
	Find fit indices from the discrepancy values of the target model and null models. This function is modified from the \code{fitMeasures} function in \code{lavaan} package
}
\usage{
fitMeasuresChi(X2, df, p, X2.null, df.null, p.null, N, fit.measures="all")
}
\arguments{
  \item{X2}{
	The chi-square value of the target model
}
  \item{df}{
	The degree of freedom of the target model
}
  \item{p}{
	The p vlaue of the target model
}
  \item{X2.null}{
	The chi-square value of the null model
}
  \item{df.null}{
	The degree of freedom of the null model
}
  \item{p.null}{
	The p value of the null model
}
  \item{N}{
	Sample size
}
  \item{fit.measures}{
	The list of selected fit measures
}
}
\value{
	A vector of fit measures
}
\author{
    Yves Rosseel in the lavaan package 
	Modified by Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\examples{
# No example
}
