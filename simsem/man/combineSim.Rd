\name{combineSim}
\alias{combineSim}
\title{Combine result objects}
\description{
	Combine result objects into a single result object
}
\usage{
combineSim(...)
}
\arguments{
  \item{\dots}{Result objects, \code{\linkS4class{SimResult}}}
}
\value{	A combined result object}
\author{
	Terry Jorgensen (University of Kansas; \email{TJorgensen314@gmail.com}), 
    Sunthud Pornprasertmanit (\email{psunthud@gmail.com})
}
\seealso{
	Result object (\code{\linkS4class{SimResult}})
}
\examples{
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
Output1 <- sim(5, CFA.Model, n=200, seed=123321)
Output2 <- sim(4, CFA.Model, n=200, seed=324567)
Output3 <- sim(3, CFA.Model, n=200, seed=789987)
Output <- combineSim(Output1, Output2, Output3)
summary(Output)
}
