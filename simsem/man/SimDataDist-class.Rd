\name{SimDataDist-class}
\Rdversion{1.1}
\docType{class}
\alias{SimDataDist-class}
\alias{summary,SimDataDist-method}
\alias{run,SimDataDist-method}
\alias{plotDist,SimDataDist-method}
\alias{extract,SimDataDist-method}
\title{Class \code{"SimDataDist"}}
\description{
This class will provide the distribution of a dataset.
}
\section{Objects from the Class}{
Objects can be created by \code{\link{simDataDist}} function. It can also be called from the form \code{new("SimDataDist", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{p}:}{Number of variables}
    \item{\code{dist}:}{The list of marginal distribution objects, which will be used in a normal copula.}
    \item{\code{keepScale}:}{Transform back to retain the mean and standard deviation of a variable equal to the model implied mean and standard deviation (with sampling error)}
    \item{\code{reverse}:}{To mirror each variable or not. If \code{TRUE}, reverse the distribution of a variable (e.g., from positive skewed to negative skewed).}
  }
}
\section{Methods}{
	\itemize{
		\item{summary}{To summarize the object}
		\item{run}{To create data from an object. There are three additional required objects: n = sample size, m = mean of variables, cm = covariance matrix of variables.}
		\item{plotDist}{To plot a density distribution (for one variable) or a contour plot (for two variables). If the object has more than two variables, the \code{var} argument can be used to select the index of plotting variables. For two variables, the default is to have correlation of 0. To change a correlation, the \code{r} argument can be used. The \code{xlim} and \code{ylim} can be specified to set the ranges of variables.}
		\item{extract}{Extract elements from an object. The next argument is the position of the object to be extracted. }
	}
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\link{simDataDist}} The constructor of this class.	
	}
}
\examples{
# Need to be fixed

showClass("SimDataDist")

chisq3 <- simChisq(3)
chisq8 <- simChisq(8)
dist <- simDataDist(chisq3, chisq8)
% dist2 <- extract(dist, 2)

m <- c(0, 0)
cm <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
n <- 20
# dat <- run(dist, n, m, cm)

plotDist(dist, r=0.2)

}
\keyword{classes}
