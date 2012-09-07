\name{bindDist}
\alias{bindDist}
\title{
	Create a data distribution object.
}
\description{
	Create a data distribution object
}
\usage{
bindDist(margins, ..., p = NULL, keepScale = TRUE, reverse = FALSE)
}
\arguments{
  \item{margins}{
	A character vector specifying all the marginal distributions. See the description of \code{margins} attribute of the \code{\link[copula]{Mvdc}} function for further details.
}
  \item{...}{
	A list whose each component is a list of named components, giving the parameter values of the marginal distributions. See the description of \code{paramMargins} attribute of the \code{\link[copula]{Mvdc}} function for further details.
}
  \item{p}{
	Number of variables. If only one distribution object is listed, the \code{p} will make the same distribution objects for all variables.
}
  \item{keepScale}{
	A vector representing whether each variable is transformed its mean and standard deviation or not. If TRUE, transform back to retain the mean and standard deviation of a variable equal to the model implied mean and standard deviation (with sampling error)
}
  \item{reverse}{
	A vector representing whether each variable is mirrored or not. If \code{TRUE}, reverse the distribution of a variable (e.g., from positive skewed to negative skewed. If one logical value is specified, it will apply to all variables.
}
}
\value{
	\code{\linkS4class{SimDataDist}} that saves analysis result from simulate data.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimResult}} for the type of resulting object
	}
}
\examples{
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
