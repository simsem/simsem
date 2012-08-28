\name{generate}
\alias{generate}
\title{
	TBA
}
\description{
	TBA
}
\usage{
generate(model, n, maxDraw=50, misfitBounds=NULL, misfitType="f0",
averageNumMisspec=FALSE, optMisfit=NULL, optDraws=50,
indDist=NULL, sequential=FALSE,
facDist=NULL, errorDist=NULL, indLab=NULL, modelBoot=FALSE, realData=NULL, 
params=FALSE)
}
\arguments{
  \item{model}{
	TBA
}
  \item{n}{
	TBA
}
  \item{maxDraw}{
	TBA
}
  \item{misfitBounds}{ 
	TBA
}
  \item{misfitType}{ 
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
  \item{params}{ 
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
}

