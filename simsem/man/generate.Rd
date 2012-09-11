\name{generate}
\alias{generate}
\title{
	Generate data using SimSem template
}
\description{
	This function can be used to generate random data based on the \code{\linkS4class{SimSem}} template. Some notable features include fine control of misspecification and misspecification optimization, as well as the ability to generate non-normal data. When using \emph{simsem} for simulations, this function is used internally to generate data in the function \code{sim}, and can be helpful for debugging, or in creating data for use with other analysis programs. 
}
\usage{
generate(model, n, maxDraw=50, misfitBounds=NULL, misfitType="f0",
	averageNumMisspec=FALSE, optMisfit=NULL, optDraws=50,
	indDist=NULL, sequential=FALSE,	facDist=NULL, errorDist=NULL, 
	indLab=NULL, modelBoot=FALSE, realData=NULL, params=FALSE)
}
\arguments{
  \item{model}{
	A \code{\linkS4class{SimSem}} object. 
}
  \item{n}{
	Integer of sample size.
}
  \item{maxDraw}{
	Integer specifying the maximum number of attempts to draw a valid set of parameters (no negative error variance, standardized coefficients over 1).
}
  \item{misfitBounds}{ 
	Vector that contains upper and lower bounds of the misfit measure. Sets of parameters drawn that are not within these bounds are rejected.
}
  \item{misfitType}{ 
	Character vector indicating the fit measure used to assess the misfit of a set of parameters. Can be "f0", "rmsea", "srmr", or "all". 
}
  \item{averageNumMisspec}{ 
	If \code{TRUE}, the provided fit will be divided by the number of misspecified parameters. 
}
  \item{optMisfit}{ 
	Character vector of either "min" or "max" indicating either maximum or minimum optimized misfit. If not null, the set of parameters out of the number of draws in "optDraws" that has either the maximum or minimum misfit of the given misfit type will be returned.
}
  \item{optDraws}{ 
	Number of parameter sets to draw if optMisfit is not null. The set of parameters with the maximum or minimum misfit will be returned.
}
  \item{indDist}{ 
	A \code{\linkS4class{SimDataDist}} object or list of objects for a distribution of indicators. If one object is passed, each indicator will have the same distribution. Use when \code{sequential} is \code{FALSE}.
}
 \item{sequential}{ 
	If \code{TRUE}, use a sequential method to create data such that the data from factor are generated first and apply to a set of equations to obtain the data of indicators. If \code{FALSE}, create data directly from model-implied mean and covariance of indicators.
}
  \item{facDist}{ 
  A \code{\linkS4class{SimDataDist}} object or list of objects for the distribution of factors. If one object is passed, all factors will have the same distribution. Use when \code{sequential} is \code{TRUE}.
}
  \item{errorDist}{ 
	An object or list of objects of type \code{SimDataDist} indicating the distribution of errors. If a single \code{SimDataDist} is specified, each error will be genrated with that distribution.
}
  \item{indLab}{ 
	A vector of indicator labels. When not specified, the variable names are \code{x1, x2, ... xN}.
}
  \item{modelBoot}{ 
	 When specified, a model-based bootstrap is used for data generation. See details for further information. This argument requires real data to be passed to \code{realData}.
}
  \item{realData}{ 
	A data.frame containing real data. The data generated will follow the distribution of this data set.
}
  \item{params}{ 
	If \code{TRUE}, return the parameters drawn along with the generated data set. Default is \code{FALSE}.
}
}
\value{
	A data.frame containing simulated data from the data generation template. A variable "group" is appended indicating group membership.
}
\details{
This function will use \code{\link[MASS]{mvrnorm}} function ito create data from model implied covariance matrix if the data distribution object (\code{\linkS4class{SimDataDist}}) is not specified. It the data distribution object is specified, the Gaussian copula model is used. See \code{\linkS4class{SimDataDist}} for further details. For the model-based bootstrap, the transformation proposed by Yung & Bentler (1996) is used. This procedure is the expansion from the Bollen and Stine (1992) bootstrap including a mean structure. The model-implied mean vector and covariance matrix with trivial misspecification will be used in the model-based bootstrap if \code{misspec} is specified. See page 133 of Bollen and Stine (1992) for a reference. 

Internally, parameters are first drawn, and data is then created from these parameters. Both of these steps are available via the \code{\link{draw}} and \code{\link{createData}} functions respectively.
}
\references{
Bollen, K. A., & Stine, R. A. (1992). Bootstrapping goodness-of-fit measures in structural equation models. \emph{Sociological Methods and Research, 21,} 205-229.

Yung, Y.-F., & Bentler, P. M. (1996). Bootstrapping techniques in analysis of mean and covariance structures. In G. A. Marcoulides & R. E. Schumacker (Eds.), \emph{Advanced structural equation modeling: Issues and techniques} (pp. 195-226). Mahwah, NJ: Erlbaum.
}
\author{	
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu}), Patrick Miller (Univeristy of Notre Dame; \email{pmille13@nd.edu})
}

\seealso{
	\itemize{
		\item \code{\link{draw}} To draw parameters using the \code{\linkS4class{SimSem}} template.
		\item \code{\link{createData}} To generate random data using a set of parameters from \code{\link{draw}}
	}
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

dat <- generate(CFA.Model,200)
}

