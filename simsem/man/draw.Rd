\name{draw}
\alias{draw}
\title{
	TBA
}
\description{
	TBA
}
\usage{
draw(model, maxDraw=50, misfitBounds=NULL, averageNumMisspec=FALSE, 
optMisfit = NULL, optDraws=50, misfitType="f0",misfitOut=FALSE)
}
\arguments{
  \item{model}{
	TBA
}
  \item{maxDraw}{
	TBA
}
  \item{misfitBounds}{
	TBA
}
  \item{averageNumMisspec}{
	TBA
}
  \item{optMisfit}{
	TBA
}
  \item{optDraws}{
	TBA
}
  \item{misfitType}{
	TBA
}
  \item{misfitOut}{
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
}
