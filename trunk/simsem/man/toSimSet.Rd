\name{toSimSet}
\alias{toSimSet}
\title{
	Transform the analysis model object into the object for data generation 
}
\description{
	Transform the analysis model object (\code{\linkS4class{SimModelOut}}) into the object for data generation (\code{\linkS4class{SimSet}})
}
\usage{
toSimSet(out, usedStd=TRUE)
}
\arguments{
  \item{out}{
	The analysis output object to be transformed (\code{\linkS4class{SimModelOut}})
}
  \item{usedStd}{
	If \code{TRUE}, the standardized estimates will be used. If \code{FALSE}, the unstandardized estimates will be used. 
}
}
\value{
	The \code{\linkS4class{SimSet}} that contains parameters for data generation.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\examples{
# This function is not public.

# library(lavaan)
# hs <- HolzingerSwineford1939
# loading <- matrix(0, 9, 3)
# loading[1:3, 1] <- NA
# loading[4:6, 2] <- NA
# loading[7:9, 3] <- NA
# model <- simParamCFA(LY=loading)
# SimModel <- simModel(model, indLab=paste("x", 1:9, sep=""))
# out <- run(SimModel, hs)
# set <- toSimSet(out)
}
