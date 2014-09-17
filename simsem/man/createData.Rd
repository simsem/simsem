\name{createData}
\alias{createData}
\title{
	Create data from a set of drawn parameters.
}
\description{
	This function can be used to create data from a set of parameters created from \code{\link{draw}}, called a code{paramSet}. This function is used internally to create data, and is available publicly for accessibility and debugging.
}
\usage{
createData(paramSet, n, indDist=NULL, sequential=FALSE, facDist=NULL, 
errorDist=NULL, saveLatentVar = FALSE, indLab=NULL, facLab = NULL, 
modelBoot=FALSE, realData=NULL, covData=NULL, empirical = FALSE)
}
\arguments{
  \item{paramSet}{
	Set of drawn parameters from \code{\link{draw}}.
}
  \item{n}{
	Integer of desired sample size.
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
  \item{saveLatentVar}{ 
	If \code{TRUE}, the total latent variable scores, residual latent variable scores, and measurement error scores are also provided as the \code{"latentVar"} attribute of the generated data by the following line: \code{attr(generatedData, "latentVar")}. The \code{sequential} argument must be \code{TRUE} in order to use this option.
}
  \item{indLab}{
	A vector of indicator labels. When not specified, the variable names are \code{x1, x2, ... xN}.
}
  \item{facLab}{
	A vector of factor labels. When not specified, the variable names are \code{f1, f2, ... fN}.
}
  \item{modelBoot}{
	When specified, a model-based bootstrap is used for data generation. See details for further information. This argument requires real data to be passed to \code{readData}.
}
  \item{realData}{
	A data.frame containing real data. The data generated will follow the distribution of this data set.
}
  \item{covData}{ 
	A data.frame containing covariate data, which can have any distributions. This argument is required when users specify \code{GA} or \code{KA} matrices in the model template (\code{\linkS4class{SimSem}}).
}
  \item{empirical}{ 
	Logical. If \code{TRUE}, the specified parameters are treated as sample statistics and data are created to get the specified sample statistics. This argument is applicable when multivariate normal distribution is specified only.
}
}
\value{
	A data.frame containing simulated data from the data generation template. A variable "group" is appended indicating group membership.
}
\details{
This function will use the modified \code{mvrnorm} function (from the MASS package) by Paul E. Johnson  to create data from model implied covariance matrix if the data distribution object (\code{\linkS4class{SimDataDist}}) is not specified. The modified function is just a small modification from the original \code{mvrnorm} function such that the data generated with the sample sizes of n and n + k (where k > 0) will be replicable in the first n rows.

It the data distribution object is specified, either the copula model or the Vale and Maurelli's method is used. For the copula approach, if the \code{copula} argument is not specified in the data distribution object, the naive Gaussian copula is used. The correlation matrix is direct applied to the multivariate Gaussian copula. The correlation matrix will be equivalent to the Spearman's correlation (rank correlation) of the resulting data. If the \code{copula} argument is specified, such as \code{\link[copula]{ellipCopula}}, \code{\link[copula]{normalCopula}}, or \code{\link[copula]{archmCopula}}, the data-transformation method from Mair, Satorra, and Bentler (2012) is used. In brief, the data (\eqn{X}) are created from the multivariate copula. The covariance from the generated data is used as the starting point (\eqn{S}). Then, the target data (\eqn{Y}) with the target covariance as model-implied covariance matrix (\eqn{\Sigma_0}) can be created:

\deqn{ Y = XS^{-1/2}\Sigma^{1/2}_0. }

See \code{\link{bindDist}} for further details. For the Vale and Maurelli's (1983) method, the code is brought from the \code{lavaan} package.

For the model-based bootstrap, the transformation proposed by Yung & Bentler (1996) is used. This procedure is the expansion from the Bollen and Stine (1992) bootstrap including a mean structure. The model-implied mean vector and covariance matrix with trivial misspecification will be used in the model-based bootstrap if \code{misspec} is specified. See page 133 of Bollen and Stine (1992) for a reference. 

Internally, parameters are first drawn, and data is then created from these parameters. Both of these steps are available via the \code{\link{draw}} and \code{\link{createData}} functions respectively.
}
\references{
Bollen, K. A., & Stine, R. A. (1992). Bootstrapping goodness-of-fit measures in structural equation models. \emph{Sociological Methods and Research, 21,} 205-229.

Mair, P., Satorra, A., & Bentler, P. M. (2012). Generating nonnormal multivariate data using copulas: Applications to SEM. \emph{Multivariate Behavioral Research, 47}, 547-565.

Vale, C. D. & Maurelli, V. A. (1983) Simulating multivariate nonormal distributions. \emph{Psychometrika, 48}, 465-471.

Yung, Y.-F., & Bentler, P. M. (1996). Bootstrapping techniques in analysis of mean and covariance structures. In G. A. Marcoulides & R. E. Schumacker (Eds.), \emph{Advanced structural equation modeling: Issues and techniques} (pp. 195-226). Mahwah, NJ: Erlbaum.
}
\author{	
    Sunthud Pornprasertmanit (Texas Tech University; \email{sunthud.pornprasertmanit@ttu.edu}), Patrick Miller (University of Notre Dame; \email{pmille13@nd.edu}). The original code of \code{mvrnorm} function is based on the \code{MASS} package slightly modified by Paul E. Johnson. The code for data-transformation in multivariate copula is based on Mair et al. (2012) article. The code for Vale and Maurelli (1983) is slightly modified from the function provided in the \code{lavaan} package.
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

# Draw a parameter set for data generation.
param <- draw(CFA.Model)

# Generate data from the first group in the paramList.
dat <- createData(param[[1]], n = 200) 
}
