\name{summaryPopulation}
\alias{summaryPopulation}
\title{
	Summarize the population model used for data generation underlying a result object
}
\description{
	Summarize the population model used for data generation underlying a result object
}
\usage{
summaryPopulation(object, std = FALSE, improper = TRUE)
}
\arguments{
  \item{object}{
	The result object that you wish to extract the data generation population model from (\code{linkS4class{SimResult}}).
}
  \item{std}{
		If \code{TRUE}, the standardized parameter values are used instead of unstandardized parameter values.
}
  \item{improper}{
	If TRUE, include the replications that provided improper solutions
}
}
\value{
	A \code{data.frame} contianing the summary of population model across replications.
}
\author{
    Sunthud Pornprasertmanit (\email{psunthud@gmail.com})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimResult}} for result object
	}
}
\examples{
\dontrun{
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LY <- bind(loading, "runif(1, 0.4, 0.9)")
RPS <- binds(diag(1))
RTE <- binds(diag(6))
CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA")

# We will use only 10 replications to save time.
# In reality, more replications are needed.
Output <- sim(10, n=200, model=CFA.Model)

# Get the summary of population model
summaryPopulation(Output)
}
}
