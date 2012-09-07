\name{SimDataDist-class}
\Rdversion{1.1}
\docType{class}
\alias{SimDataDist-class}
\alias{summary,SimDataDist-method}
\alias{plotDist,SimDataDist-method}
\title{Class \code{"SimDataDist"}}
\description{
This class will provide the distribution of a dataset.
}
\section{Objects from the Class}{
Objects can be created by \code{\link{bindDist}} function. It can also be called from the form \code{new("SimDataDist", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{p}:}{Number of variables}
    \item{\code{margins}:}{A character vector specifying all the marginal distributions}
    \item{\code{paramMargins}:}{A list whose each component is a list of named components, giving the parameter values of the marginal distributions.}
    \item{\code{keepScale}:}{Transform back to retain the mean and standard deviation of a variable equal to the model implied mean and standard deviation (with sampling error)}
    \item{\code{reverse}:}{To mirror each variable or not. If \code{TRUE}, reverse the distribution of a variable (e.g., from positive skewed to negative skewed).}
  }
}
\section{Methods}{
	\itemize{
		\item{summary}{To summarize the object}
		\item{plotDist}{To plot a density distribution (for one variable) or a contour plot (for two variables). If the object has more than two variables, the \code{var} argument can be used to select the index of plotting variables. For two variables, the default is to have correlation of 0. To change a correlation, the \code{r} argument can be used. The \code{xlim} and \code{ylim} can be specified to set the ranges of variables.}
		\item{extract}{Extract elements from an object. The next argument is the position of the object to be extracted. }
	}
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\link{bindDist}} The constructor of this class.	
	}
}
\examples{
# Need to be fixed

showClass("SimDataDist")

d1 <- list(df=2)
d2 <- list(df=3)
d3 <- list(df=4)
d4 <- list(df=5)
d5 <- list(df=3)
d6 <- list(df=4)
d7 <- list(df=5)
d8 <- list(df=6)


dist <- bindDist(c(rep("t", 4), rep("chisq", 8)), d1, d2, d3, d4, d5, d6, d7, d8, d5, d6, d7, d8)

}
\keyword{classes}
