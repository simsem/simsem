\name{simData}
\alias{simData}
\alias{simData-methods}
\alias{simData,ANY-method}
\alias{simData,SimSet-method}
\alias{simData,SimModelOut-method}
\alias{simData,SimRSet-method}
\title{
	Data object
}
\description{
	This function will be used to create data specification and ready for data simulation.
}
\usage{
simData(param, ...)
}
\arguments{
  \item{param}{
	Model specification matrices that are created by \code{\link{simSetCFA}}, \code{\link{simSetPath}}, or \code{\link{simSetSEM}} or the analysis output.
}
  \item{\dots}{
	Other values that will be explained specifically for each class
}
}
\value{
	\linkS4class{SimData} object that save data model specification.
}
\section{Details in \dots}{
	\itemize{
		\item \emph{n:} Desired sample size
		\item \emph{misspec:} Model \emph{misspecification} matrices that are created by \code{\link{simMisspecCFA}}, \code{\link{simMisspecPath}}, or \code{\link{simMisspecSEM}}.
		\item \emph{equalCon:} Equality constraints that are created by \code{\link{simEqualCon}}. This will specify equality econstraints of parameters in data generation process.
		\item \emph{maxDraw:} The maximum number of random drawn parameters and misspecification model until all parameters in the model are eligible (no negative error variance, standardized coefficients over 1).
		\item \emph{sequential:} If \code{TRUE}, use a sequential method to create data such that the data from factor are generated first and apply to a set of equations to obtain the data of indicators. If \code{FALSE}, create data directly from model-implied mean and covariance of indicators.
		\item \emph{facDist:} A \code{\linkS4class{SimDataDist}} for a distribution of factors. Use when \code{sequential} is \code{TRUE}.
		\item \emph{errorDist:} A \code{\linkS4class{SimDataDist}} for a distribution of measurement errors. Use when \code{sequential} is \code{TRUE}
		\item \emph{indDist:} A \code{\linkS4class{SimDataDist}} for a distribution of indicators. Use when \code{sequential} is \code{FALSE}.
		\item \emph{indLab:} A vector of indicator names. If not specified, the variables names are y1, y2, ... .
		\item \emph{modelBoot:} If \code{TRUE}, use a model-based bootstrap for data generation. See details for further information. This argument need a dataset in the \code{realData} argument.
		\item \emph{realData:} The real dataset that the model based bootstrap will follow the distribution.
		\item \emph{usedStd:} If \code{\linkS4class{SimModelOut}} is used for data generation, standardized parameters are used for data generation if \code{usedStd=TRUE}. If \code{usedStd=FALSE}, unstandardized parameters are used.
	}
}
%%%%%%%%%%%%%% Check link %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\details{
	This function will use \code{mvrnorm} function in \code{MASS} package to create data from model implied covariance matrix if the data distribution object (\code{\linkS4class{SimDataDist}}) is not specified. It the data distribution object is specified, the Gaussian copula model is used. See \code{\linkS4class{SimDataDist}} for further details. For the model-based bootstrap, the transformation proposed by Yung & Bentler (1996) is used. This procedure is the expansion from the Bollen and Stine (1992) bootstrap including a mean structure. The model-implied mean vector and covariance matrix with trivial misspecification will be used in the model-based bootstrap if \code{misspec} is specified. See page 133 of Bollen and Stine (1992) for a reference. 
}
\references{
	\itemize{
		\item Bollen, K. A., & Stine, R. A. (1992). Bootstrapping goodness-of-fit measures in structural equation models. \emph{Sociological Methods and Research, 21,} 205-229.
		\item Yung, Y.-F., & Bentler, P. M. (1996). Bootstrapping techniques in analysis of mean and covariance structures. In G. A. Marcoulides & R. E. Schumacker (Eds.), \emph{Advanced structural equation modeling: Issues and techniques} (pp. 195-226). Mahwah, NJ: Erlbaum.
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
		\item \code{\link{simDataDist}} for data distribution object set-up for \code{facDist}, \code{errorDist}, or \code{indDist} arguments.
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
SimData <- simData(CFA.Model, 200)
summary(SimData)
run(SimData)

# With Misspecification Model
n01 <- simNorm(0, 0.1)
error.cor.Mis <- matrix(NA, 6, 6)
diag(error.cor.Mis) <- 1
RTD.Mis <- symMatrix(error.cor.Mis, "n01")
CFA.Model.Mis <- simMisspecCFA(RTD=RTD.Mis)
SimData <- simData(CFA.Model, 200, misspec=CFA.Model.Mis)
summary(SimData)
run(SimData)
}
