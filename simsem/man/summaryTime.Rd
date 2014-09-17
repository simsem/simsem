\name{summaryTime}
\alias{summaryTime}
\title{
Time summary
}
\description{
Provide a summary of time elapsed in running the simulation.
}
\usage{
summaryTime(object, units = "seconds")
}
\arguments{
  \item{object}{
	\code{\linkS4class{SimResult}} object being described
}
  \item{units}{
	The units of time, which can be "seconds", "minutes", "hours", or "days"
}
}
\value{
The first section is the actual time used in each step of the simulation. The second section is the average system (processor) time used in each replication. The third section is the summary of starting time, end time, total actual time, and total system time.
}
\author{
    Sunthud Pornprasertmanit (Texas Tech University; \email{sunthud.pornprasertmanit@ttu.edu})
}
\examples{
\dontrun{
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LY <- bind(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

RTE <- binds(diag(6))

VY <- bind(rep(NA,6),2)

CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType = "CFA")

# In reality, more than 5 replications are needed.
Output <- sim(5, CFA.Model, n=200)
summaryTime(Output)
}
}
