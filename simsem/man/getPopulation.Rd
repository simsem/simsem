\name{getPopulation}
\alias{getPopulation}
\title{
	Extract the data generation population model underlying a result object
}
\description{
This function will extract the data generation population model underlying a result object (\code{linkS4class{SimResult}}).
}
\usage{
getPopulation(object)
}
\arguments{
  \item{object}{
	The result object that you wish to extract the data generation population model from (\code{linkS4class{SimResult}}).
}
}
\value{
	A data frame contained the population of each replication
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
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
LX <- bind(loading, "runif(1, 0.4, 0.9)")
RPH <- binds(diag(1))
RTD <- binds(diag(6))
CFA.Model <- model(LY = LX, RPS = RPH, RTE = RTD, modelType="CFA")

# We will use only 10 replications to save time.
# In reality, more replications are needed.
Output <- sim(10, n=200, model=CFA.Model)
getPopulation(Output)
}
}
