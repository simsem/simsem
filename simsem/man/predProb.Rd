\name{predProb}
\alias{predProb}
\title{
	Function to get predicted probabilities from logistic regression
}
\description{
	Function to get predicted probabilities from logistic regression
}
\usage{
predProb(newdat, glmObj)
}
\arguments{
\item{newdat}{
	A vector of values for all predictors, including the intercept
}
  \item{glmObj}{
	An object from a fitted glm run with a logit link
}
}
\value{
	Predictive probability of success given the values in the \code{newdat} argument.
}
\author{
    Alexander M. Schoemann (University of Kansas; \email{schoemann@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\link{continuousPower}} 
		\item \code{\link{getPower}} 		
	}
}
\examples{
# No example
}
