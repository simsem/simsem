\name{simData}
\alias{simData}
\title{
	Data object
}
\description{
	This function will be used to create data specification and ready for data simulation.
}
\usage{
simData(n, param, misspec = new("NullSimMisspec"), equalCon = new("NullSimEqualCon"), conBeforeMis = TRUE, misfitBound = new("NullVector"), maxDraw = 100,  sequential=NA, facDist=new("NullSimDataDist"), errorDist=new("NullSimDataDist"), indDist=new("NullSimDataDist"))
}
\arguments{
  \item{n}{
	Desired sample size
}
  \item{param}{
	Model specification matrices that are created by \code{\link{simSetCFA}}, \code{\link{simSetPath}}, or \code{\link{simSetSEM}}.
}
  \item{misspec}{
	Model \emph{misspecification} matrices that are created by \code{\link{simMisspecCFA}}, \code{\link{simMisspecPath}}, or \code{\link{simMisspecSEM}}.
}
  \item{equalCon}{
	Equality constraints that are created by \code{\link{simEqualCon}}. This will specify equality econstraints of parameters in data generation process.
}
  \item{conBeforeMis}{
	\code{TRUE} if users wish to constrain parameters before adding misspecification. 
	\code{FALSE} if users wish to constrain parameters after adding misspecification.
}
  \item{misfitBound}{
	Upper bound of population root mean squared error of approximation (RMSEA; Browne & Cudeck, 1992) that users wish their model misspecification to be
}
  \item{maxDraw}{
	The maximum number of random drawn parameters and misspecification model until all parameters in the model are eligible 
	(no negative error variance, standardized coefficients over 1).
}
   \item{sequential}{
   If \code{TRUE}, use a sequential method to create data such that the data from factor are generated first and apply to a set of equations to obtain the data of indicators. If \code{FALSE}, create data directly from model-implied mean and covariance of indicators.
}
    \item{facDist}{
	A \code{\linkS4class{SimDataDist}} for a distribution of factors. Use when \code{sequential} is \code{TRUE}.
}
    \item{errorDist}{
	A \code{\linkS4class{SimDataDist}} for a distribution of measurement errors. Use when \code{sequential} is \code{TRUE}
}
    \item{indDist}{
	A \code{\linkS4class{SimDataDist}} for a distribution of indicators. Use when \code{sequential} is \code{FALSE}.
}
}
%%%%%%%%%%%%%% Check link %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\details{
	This function will use \code{mvrnorm} function in \code{MASS} package to create data from model implied covariance matrix. 
}
\value{
	\linkS4class{SimData} object that save data model specification.
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
	\itemize{
		\item \code{\link{simSetCFA}} to see CFA model specification
		\item \code{\link{simSetPath}} to see Path analysis model specification
		\item \code{\link{simSetSEM}} to see SEM model specification
		\item \code{\link{simMisspecCFA}} for specifying misspecification in CFA model
		\item \code{\link{simMisspecPath}} for specifying misspecification in Path analysis model
		\item \code{\link{simMisspecSEM}}  for specifying misspecification in SEM model
		\item \code{\link{simEqualCon}} for setting equality constraints.
	}

}
\examples{

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- simMatrix(loading, loadingValues)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, 0.5)
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
RTD <- symMatrix(error.cor)
CFA.Model <- simSetCFA(LY = LX, RPS = RPH, RTE = RTD)
SimData <- simData(200, CFA.Model)
summary(SimData)
run(SimData)

# With Misspecification Model
n01 <- simNorm(0, 0.1)
error.cor.Mis <- matrix(NA, 6, 6)
diag(error.cor.Mis) <- 1
RTD.Mis <- symMatrix(error.cor.Mis, "n01")
CFA.Model.Mis <- simMisspecCFA(RTD=RTD.Mis)
SimData <- simData(200, CFA.Model, misspec=CFA.Model.Mis)
summary(SimData)
run(SimData)
}
