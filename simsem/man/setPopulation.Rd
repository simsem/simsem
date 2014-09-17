\name{setPopulation}
\alias{setPopulation}
\title{
	Set the data generation population model underlying an object
}
\description{
This function will set the data generation population model to be an appropriate one. If the appropriate data generation model is specified, the additional features can be seen in \code{\link{summary}} or \code{\link{summaryParam}} functions on the target object, such as bias in parameter estimates or percentage coverage. 
}
\usage{
setPopulation(target, population)
}
\arguments{
  \item{target}{
	The result object that you wish to set the data generation population model (\code{linkS4class{SimResult}}).
}
  \item{population}{
	The population parameters specified in the \code{linkS4class{SimSem}} object
}
}
\value{
	The target object that is changed the parameter.
}
\author{
    Sunthud Pornprasertmanit (Texas Tech University; \email{sunthud.pornprasertmanit@ttu.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimResult}} for result object
	}
}
\examples{
# See each class for an example.
\dontrun{
# Data generation model
loading <- matrix(0, 7, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[1:7, 3] <- NA
loadingVal <- matrix(0, 7, 3)
loadingVal[1:3, 1] <- "runif(1, 0.5, 0.7)"
loadingVal[4:6, 2] <- "runif(1, 0.5, 0.7)"
loadingVal[1:6, 3] <- "runif(1, 0.3, 0.5)"
loadingVal[7, 3] <- 1
loading.mis <- matrix("runif(1, -0.2, 0.2)", 7, 3)
loading.mis[is.na(loading)] <- 0
loading.mis[,3] <- 0
loading.mis[7,] <- 0
LY <- bind(loading, loadingVal, misspec=loading.mis)

RPS <- binds(diag(3))

path <- matrix(0, 3, 3)
path[2, 1] <- NA
BE <- bind(path, "runif(1, 0.3, 0.5)")

RTE <- binds(diag(7))

VY <- bind(c(rep(NA, 6), 0), c(rep(1, 6), ""))

datamodel <- model(LY=LY, RPS=RPS, BE=BE, RTE=RTE, VY=VY, modelType="SEM")

# Data analysis model
loading <- matrix(0, 7, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7, 3] <- NA
path <- matrix(0, 3, 3)
path[2, 1] <- NA
path[1, 3] <- NA
path[2, 3] <- NA
errorCov <- diag(NA, 7)
errorCov[7, 7] <- 0
facCov <- diag(3)
analysis <- estmodel(LY=loading, BE=path, TE=errorCov, PS=facCov, modelType="SEM", 
	indLab=paste("y", 1:7, sep=""))

# In reality, more than 10 replications are needed.
Output <- sim(10, n=200, analysis, generate=datamodel)

# Population 
loadingVal <- matrix(0, 7, 3)
loadingVal[1:3, 1] <- 0.6
loadingVal[4:6, 2] <- 0.6
loadingVal[7, 3] <- 1
LY <- bind(loading, loadingVal)
pathVal <- matrix(0, 3, 3)
pathVal[2, 1] <- 0.4
pathVal[1, 3] <- 0.4
pathVal[2, 3] <- 0.4
BE <- bind(path, pathVal)
PS <- binds(facCov)
errorCovVal <- diag(0.64, 7)
errorCovVal[7, 7] <- 0
TE <- binds(errorCov, errorCovVal)
population <- model(LY=LY, PS=PS, BE=BE, TE=TE, modelType="SEM")

# Set up the new population
Output <- setPopulation(Output, population) 

# This summary will contain the bias information
summary(Output)
}
}
