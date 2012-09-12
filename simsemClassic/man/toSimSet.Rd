\name{toSimSet}
\alias{toSimSet}
\alias{toSimSet-methods}
\alias{toSimSet,ANY-method}
\alias{toSimSet,SimRSet-method}
\alias{toSimSet,SimModelOut-method}
\title{
	Transform the analysis model object into the object for data generation 
}
\description{
	Transform an analysis model object (\code{\linkS4class{SimModelOut}}) or reduced parameters object (\code{\linkS4class{SimRSet}}) into the object for data generation (\code{\linkS4class{SimSet}})
}
\usage{
toSimSet(out, ...)
}
\arguments{
  \item{out}{
	The analysis output object to be transformed (\code{\linkS4class{SimModelOut}})
}
  \item{\dots}{
	An additional argument
}
}
\section{Methods}{
	\describe{
	\item{signature(out = "SimModelOut")}{
		This function transforms an analysis model object into the object for data generation. The additional argument is \code{usedStd}. If \code{usedStd=TRUE}, the standardized estimates will be used. If \code{usedStd=FALSE}, the unstandardized estimates will be used. 	
	}
	\item{signature(out = "SimRSet")}{
		This function transforms a parameter value object (in data analysis parameterization) into the object for data generation. The additional argument is \code{param}, which is the \code{\linkS4class{SimParam}} saving the free parameters specification.
	}
}}


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
