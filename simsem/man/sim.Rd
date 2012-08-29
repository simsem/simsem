\name{sim}
\alias{sim}
\title{
	TBA
}
\description{
	TBA
}
\usage{
sim(nRep, model, n, generate = NULL, rawData = NULL,
miss = NULL, fun=NULL,
pmMCAR = NULL, pmMAR = NULL,
facDist = NULL, indDist = NULL, errorDist = NULL, sequential = FALSE, 
modelBoot = FALSE, realData = NULL, maxDraw = 50, misfitType = "f0", 
misfitBounds = NULL, averageNumMisspec = NULL, optMisfit=NULL, optDraws = 50, 
aux = NULL, 
seed = 123321, silent = FALSE, multicore = FALSE, cluster = FALSE, numProc = NULL,  
paramOnly = FALSE, dataOnly=FALSE, ...)
}
\arguments{
  \item{nRep}{
	TBA
}
  \item{model}{
	TBA
}
  \item{n}{
	TBA
}
  \item{generate}{
	TBA
}
  \item{rawData}{
	TBA
}
  \item{miss}{
	TBA
}
  \item{fun}{
	TBA
}
  \item{pmMCAR}{
	TBA
}
  \item{pmMAR}{
	TBA
}
  \item{facDist}{ 
	TBA
}
  \item{indDist}{ 
	TBA
}
  \item{errorDist}{ 
	TBA
}
  \item{sequential}{ 
	TBA
}
  \item{modelBoot}{ 
	TBA
}
  \item{realData}{ 
	TBA
}
  \item{maxDraw}{ 
	TBA
}
  \item{misfitType}{ 
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
  \item{aux}{ 
	TBA
}
  \item{seed}{ 
	TBA
}
  \item{silent}{ 
	TBA
}
  \item{multicore}{ 
	TBA
}
  \item{cluster}{ 
	TBA
}
  \item{numProc}{ 
	TBA
}
  \item{paramOnly}{ 
	TBA
}
  \item{dataOnly}{ 
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

Output <- sim(20, CFA.Model,n=200)
summary(Output)
}

