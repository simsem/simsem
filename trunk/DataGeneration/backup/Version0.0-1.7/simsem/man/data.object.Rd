\name{data.object}
\alias{data.object}
\title{
	Data object
}
\description{
	This function will be used to create data specification and ready for data simulation.
}
\usage{
data.object(N, simMatrixSet, simMisspecifiedSet = new("nullSimMisspecifiedSet"), simConstraint = new("nullSimConstraint"), Constrain.Parameters.Only = TRUE, Misfit.bound = new("nullVector"), Maximum.random = 100)
}
\arguments{
  \item{N}{
	Desired sample size
}
  \item{simMatrixSet}{
	Model specification matrices that are created by \code{\link{matrix.CFA.object}}, \code{\link{matrix.Path.object}}, or \code{\link{matrix.SEM.object}}.
}
  \item{simMisspecifiedSet}{
	Model \emph{misspecification} matrices that are created by \code{\link{misspecified.CFA.object}}, \code{\link{misspecified.Path.object}}, or \code{\link{misspecified.SEM.object}}.
}
  \item{simConstraint}{
	Equality constraints that are created by \code{\link{constraint.object}}. This will specify equality econstraints of parameters in data generation process.
}
  \item{Constrain.Parameters.Only}{
	\code{TRUE} if users wish to constrain parameters before adding misspecification. 
	\code{FALSE} if users wish to constrain parameters after adding misspecification.
}
  \item{Misfit.bound}{
	Upper bound of population root mean squared error of approximation (RMSEA; Browne & Cudeck, 1992) that users wish their model misspecification to be
}
  \item{Maximum.random}{
	The maximum number of random drawn parameters and misspecification model until all parameters in the model are eligible 
	(no negative error variance, standardized coefficients over 1).
}
}
%%%%%%%%%%%%%% Check link %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\details{
	This function will use \code{mvrnorm} function in \code{MASS} package to create data from model implied covariance matrix. 
}
\value{
	\linkS4class{simData} object that save data model specification.
}
\references{
	\itemize{
	\item Stieger, J. H. & Lind, J. C. (1980). \emph{Statistically based tests for the number of factors}. Paper presented at the annual spring meeting of the Psychometric Society, Iowa City, IA. 
	\item Browne, M. W., & Cudeck, R. (1992). Alternative ways of assessing model fit. \emph{Sociological Methods & Research, 21}, 230-258.
	}
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\enumerate{
		\item \code{\link{matrix.CFA.object}} to see CFA model specification
		\item \code{\link{matrix.Path.object}} to see Path analysis model specification
		\item \code{\link{matrix.SEM.object}} to see SEM model specification
		\item \code{\link{misspecified.CFA.object}} for specifying misspecification in CFA model
		\item \code{\link{misspecified.Path.object}} for specifying misspecification in Path analysis model
		\item \code{\link{misspecified.SEM.object}}  for specifying misspecification in SEM model
		\item \code{\link{constraint.object}} for setting equality constraints.
	}

}
\examples{

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- matrix.object(loading, loadingValues)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
PH <- sym.matrix.object(latent.cor, 0.5)
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
TD <- sym.matrix.object(error.cor)
CFA.Model <- matrix.CFA.object(LY = LX, PS = PH, TE = TD)
SimData <- data.object(200, CFA.Model)
summary(SimData)
run(SimData)

# With Misspecification Model
n01 <- rnorm.object(0, 0.1)
error.cor.Mis <- matrix(NA, 6, 6)
diag(error.cor.Mis) <- 1
TD.Mis <- sym.matrix.object(error.cor.Mis, "n01")
CFA.Model.Mis <- misspecified.CFA.object(TD=TD.Mis)
SimData <- data.object(200, CFA.Model, simMisspecifiedSet=CFA.Model.Mis)
summary(SimData)
run(SimData)
}
