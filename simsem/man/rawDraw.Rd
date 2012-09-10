\name{rawDraw}
\alias{rawDraw}
\title{
	Draw values from vector or matrix objects
}
\description{
	Takes one matrix or vector object (\code{\linkS4class{SimMatrix}} or \code{\linkS4class{SimVector}}) and returns a matrix or a vector with numerical values for population parameters. If a matrix is symmetric, it is arbitrarily chosen that parameters on the upper triangular elements are set equal to the parameters on the lower triangular elements.
}
\usage{
rawDraw(simDat, constraint = TRUE, misSpec = TRUE, parMisOnly = FALSE, 
    misOnly = FALSE)
}
\arguments{
  \item{simDat}{
	A matrix or vector object (\code{\linkS4class{SimMatrix}} or \code{\linkS4class{SimVector}})
}
  \item{constraint}{
	If TRUE, then constraints are applied simultaneously
}
  \item{misSpec}{
	If TRUE, then a list is returned with [[1]] parameters with no misspec and [[2]] same parameters + misspec (if any)
}
  \item{parMisOnly}{
	If TRUE, then only the parameters + misspecification is returned
}
  \item{misOnly}{
	If TRUE, then only the misspecification is returned
}
}
\value{
	A matrix (or vector) or a list of matrices (or vectors) which contains the draw result.
}
\author{
	Patrick Miller (Univeristy of Notre Dame; \email{pmille13@nd.edu})
}
\examples{
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
loading[1:3, 1] <- "con1"
LY <- bind(loading, loadingVal, misspec=loading.mis)

# Draw values
rawDraw(LY)

# Draw only model parameters containing misspecification
rawDraw(LY, parMisOnly=TRUE)

# Draw only misspecification.
rawDraw(LY, misOnly=TRUE)
}
