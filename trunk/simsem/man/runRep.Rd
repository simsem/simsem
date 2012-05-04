\name{runRep}
\alias{runRep}
\title{
	Run one replication within a big simulation study
}
\description{
	Run one replication within a big simulation study within the \code{\link{simResult}} function.
}
\usage{
runRep(object, objData, objModel, objMissing=new("NullSimMissing"), objFunction=new("NullSimFunction"), silent=FALSE)
}
\arguments{
  \item{object}{
	A list of varying parts across replications: 1) list of real parameters and model misspecification, 2) sample size, 3) percent missing completely at random, 4) percent missing at random, 5) seed number
}
  \item{objData}{
	The data object, \code{\linkS4class{SimData}}, used in the simulation study
}
  \item{objModel}{
	The model object, \code{\linkS4class{SimModel}}, used in the simulation study
}
  \item{objMissing}{
	The missing object, \code{\linkS4class{SimMissing}}, used in the simulation study
}
  \item{objFunction}{
	The function object, \code{\linkS4class{SimFunction}}, used in the simulation study
}
  \item{silent}{
	If \code{TRUE}, no warning or printout as much as possible
}
}
\value{
	A list that contains
	\itemize{
		\item \code{coef} parameter estimates
		\item \code{se} standard errors
		\item \code{fit} Model fit indices
		\item \code{converged} Converged?
		\item \code{param} Parameter values provided from model output object
		\item \code{FMI1} Fraction missing method 1
		\item \code{FMI2} Fraction missing method 2
		\item \code{std} Standardized coeffcient
		\item \code{paramData} Parameter underlying generated data
	}
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
}
\examples{
# No example
}
