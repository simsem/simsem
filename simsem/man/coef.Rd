\name{coef}
\alias{coef,SimResult-method}
\title{
	Extract parameter estimates from a simulation result
}
\description{
	Extract parameter estimates from a simulation result. This function is similar to the \code{\link[=inspect,SimResult-method]{inspect}} method with \code{what = "coef"}.
}
\arguments{
  \item{object}{
	The target \code{\linkS4class{SimResult}} object
}
  \item{improper}{
    Specify whether to include the information from the replications with improper solutions
}
  \item{nonconverged}{
    Specify whether to include the information from the nonconvergent replications
}
}
\value{
	Parameter estimates of each replication
}
\author{
	Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\code{\linkS4class{SimResult}} for the object input
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
coef(Output)
coef(Output, improper = TRUE)
}
}
