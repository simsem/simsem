\name{summarySeed}
\alias{summarySeed}
\title{
Summary of a seed number
}
\description{
Summary of a seed number used in the simulation
}
\usage{
summarySeed(object)
}
\arguments{
  \item{object}{
	\code{\linkS4class{SimResult}} object being described
}
}
\value{
The first section is the seed number used in running the whole simulation. The second section is the L'Ecuyer seed of the last replication.
}
\author{
    Sunthud Pornprasertmanit (\email{psunthud@gmail.com})
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
summarySeed(Output)
}
}
