\name{simResultParam}
\alias{simResultParam}
\title{
	The constructor of the parameter result object
}
\description{
	This function is used to draw actual and misspecified paraemters in multiple replications. This function will be used to investigate the population misfits.
}
\usage{
simResultParam(nRep, object, misspec=new("NullSimMisspec"), SimEqualCon = new("NullSimEqualCon"), seed=123321, maxDraw=100)
}
\arguments{
  \item{nRep}{
	The number of drawn (replications)
}
  \item{object}{
	The \code{\linkS4class{SimSet}} class that saves the specification of actual parameters
}
  \item{misspec}{
	The \code{\linkS4class{SimMisspec}} class that saves the specification of misspecified parameters
}
  \item{SimEqualCon}{
	The \code{\linkS4class{SimEqualCon}} object that saves the equalit constraint
}
  \item{seed}{
	The seed number used to draw parameters
}
  \item{maxDraw}{
	The maximum number of sample drawn. This function will draw sets of actual and misspecified parameters repeatedly until the identified sets of actual and misspecified parameters are drawn. The maximum of repetition is specified by this argument.
}
}
\value{
	The parameter result object, \code{\linkS4class{SimResultParam}}
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\examples{
u35 <- simUnif(0.3, 0.5)
u57 <- simUnif(0.5, 0.7)
u1 <- simUnif(-0.1, 0.1)
n31 <- simNorm(0.3, 0.1)

path.BE <- matrix(0, 4, 4)
path.BE[3, 1:2] <- NA
path.BE[4, 3] <- NA
starting.BE <- matrix("", 4, 4)
starting.BE[3, 1:2] <- "u35"
starting.BE[4, 3] <- "u57"
BE <- simMatrix(path.BE, starting.BE)

residual.error <- diag(4)
residual.error[1,2] <- residual.error[2,1] <- NA
RPS <- symMatrix(residual.error, "n31")

ME <- simVector(rep(NA, 4), 0)

Path.Model <- simSetPath(RPS = RPS, BE = BE, ME = ME)

mis.path.BE <- matrix(0, 4, 4)
mis.path.BE[4, 1:2] <- NA
mis.BE <- simMatrix(mis.path.BE, "u1")
Path.Mis.Model <- simMisspecPath(BE = mis.BE, misfitType="rmsea")

# The number of replications in actual analysis should be much more than 50
ParamObject <- simResultParam(50, Path.Model, Path.Mis.Model)

# Specify the range of misfits to select the set of misspecified parameters
Path.Mis.Model2 <- simMisspecPath(BE = mis.BE, misfitType="rmsea", misfitBound=c(0.05, 0.08))
ParamObject2 <- simResultParam(50, Path.Model, Path.Mis.Model2)

# Find the maximum misspecification for each actual parameter
Path.Mis.Model3 <- simMisspecPath(BE = mis.BE, misfitType="rmsea", optMisfit="max", numIter=10)
ParamObject3 <- simResultParam(50, Path.Model, Path.Mis.Model3)
}
