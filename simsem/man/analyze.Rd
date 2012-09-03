\name{analyze}
\alias{analyze}
\title{
	TBA
}
\description{
	TBA
}
\usage{
analyze(model, data, package="lavaan", miss=NULL, indLab=NULL, 
aux=NULL, ...)
}
\arguments{
  \item{model}{
	TBA
}
  \item{data}{
	TBA
}
  \item{package}{
	TBA
}
  \item{miss}{ 
	TBA
}
  \item{indLab}{ 
	TBA
}
  \item{aux}{ 
	TBA
}
  \item{\dots}{ 
	TBA
}
}
\value{
	TBA
}
\author{
	Patrick Miller (Univeristy of Notre Dame; \email{pmille13@nd.edu})
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

dat <- generate(CFA.Model,200)
out <- analyze(CFA.Model,dat)
}

