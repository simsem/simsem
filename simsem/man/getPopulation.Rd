\name{getPopulation}
\alias{getPopulation}
\title{
	Extract the data generation population model underlying a result object
}
\description{
This function will extract the data generation population model underlying a result object (\code{linkS4class{SimResult}}).
}
\usage{
getPopulation(object, std = FALSE, improper = FALSE, nonconverged = FALSE)
}
\arguments{
  \item{object}{
	The result object that you wish to extract the data generation population model from (\code{linkS4class{SimResult}}).
}
  \item{std}{
	If \code{TRUE}, standardized parameters are returned.
}
  \item{improper}{
    Specify whether to include the information from the replications with improper solutions
}
  \item{nonconverged}{
    Specify whether to include the information from the nonconvergent replications
}
}
\value{
	A data frame contained the population of each replication
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

# Get the population parameters
getPopulation(Output)
}
}
