\name{createData}
\alias{createData}
\title{
	TBA
}
\description{
	TBA
}
\usage{
createData(paramSet, n, indDist=NULL, sequential=FALSE, facDist=NULL, 
errorDist=NULL, indLab=NULL, modelBoot=FALSE, realData=NULL)
}
\arguments{
  \item{paramSet}{
	TBA
}
  \item{n}{
	Sample size
}
  \item{indDist}{
	TBA
}
  \item{sequential}{
	TBA
}
  \item{facDist}{
	TBA
}
  \item{errorDist}{
	TBA
}
  \item{indLab}{
	TBA
}
  \item{modelBoot}{
	TBA
}
  \item{realData}{
	TBA
}
}
\value{
	TBA
}
\author{
	Patrick Miller (Univeristy of Notre Dame; \email{pmille13@nd.edu}), 
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
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

param <- draw(CFA.Model)

dat <- createData(param[[1]], n = 200) 
# Get the first-group parameter from the param object
}
