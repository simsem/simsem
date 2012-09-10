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
	A character vector specifying all the marginal distributions. The characters in argument margins are used to construct density, distribution, and quantile function names. For example, \code{"norm"} can be used to specify marginal distribution, because \code{"dnorm"}, \code{"pnorm"}, and \code{"qnorm"} are all available. A user-defined distribution or other distributions can be used. For example, \code{"gl"} function in the \code{"gld"} package can be used to represent the generalized lambda distribution where \code{"dgl"}, \code{"pgl"}, and \code{"qgl"} are available. See the description of \code{margins} attribute of the \code{\link[copula]{Mvdc}} function for further details.
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
# Create three-dimensional distribution by gaussian copula with 
# the following marginal distributions
#   1. t-distribution with df = 2
# 	2. chi-square distribution with df = 3
#	3. normal distribution with mean = 0 and sd = 1

# Setting the attribute of each marginal distribution
d1 <- list(df=2)
d2 <- list(df=3)
d3 <- list(mean=0, sd=1)

# Create a data distribution object by setting the names of each distribution
# and their arguments
dist <- bindDist(c("t", "chisq", "norm"), d1, d2, d3)
}
