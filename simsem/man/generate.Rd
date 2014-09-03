\name{generate}
\alias{generate}
\title{
	Generate data using SimSem template
}
\description{
	This function can be used to generate random data based on the 1. \code{\linkS4class{SimSem}} objects created with the \code{\link{model}} function, 2. \code{lavaan} script or parameter tables, or 3. an \code{MxModel} object from the \code{OpenMx} package. Some notable features include fine control of misspecification and misspecification optimization (for \code{\linkS4class{SimSem}} only), as well as the ability to generate non-normal data. When using \emph{simsem} for simulations, this function is used internally to generate data in the function \code{sim}, and can be helpful for debugging, or in creating data for use with other analysis programs. 
}
\usage{
generate(model, n, maxDraw=50, misfitBounds=NULL, misfitType="f0",
	averageNumMisspec=FALSE, optMisfit=NULL, optDraws=50, 
	createOrder = c(1, 2, 3), indDist=NULL, sequential=FALSE,	
	facDist=NULL, errorDist=NULL, saveLatentVar = FALSE, indLab=NULL, 
	modelBoot=FALSE, realData=NULL, covData=NULL, params=FALSE, group = NULL, 
	empirical = FALSE, ...)
}
\arguments{
  \item{model}{
	A \code{\linkS4class{SimSem}} object, a \code{lavaan} script or parameter tables, or an \code{MxModel} object from the \code{OpenMx} package
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
  \item{createOrder}{
    The order of 1) applying equality/inequality constraints, 2) applying misspecification, and 3) fill unspecified parameters (e.g., residual variances when total variances are specified). The specification of this argument is a vector of different orders of 1 (constraint), 2 (misspecification), and 3 (filling parameters). For example, \code{c(1, 2, 3)} is to apply constraints first, then add the misspecification, and finally fill all parameters. See the example of how to use it in the \code{\link{draw}} function. 
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
  \item{modelBoot}{ 
	 When specified, a model-based bootstrap is used for data generation. See details for further information. This argument requires real data to be passed to \code{realData}.
}
  \item{realData}{ 
	A data.frame containing real data. The data generated will follow the distribution of this data set.
}
  \item{covData}{ 
	A data.frame containing covariate data, which can have any distributions. This argument is required when users specify \code{GA} or \code{KA} matrices in the model template (\code{\linkS4class{SimSem}}).
}
  \item{params}{ 
	If \code{TRUE}, return the parameters drawn along with the generated data set. Default is \code{FALSE}.
}
  \item{group}{ 
	The label of the grouping variable
}
  \item{empirical}{ 
	Logical. If \code{TRUE}, the specified parameters are treated as sample statistics and data are created to get the specified sample statistics. This argument is applicable when multivariate normal distribution is specified only.
}
  \item{\dots}{ 
	Additional arguments for the \code{\link[lavaan]{simulateData}} function. 
}
}
\value{
	A data.frame containing simulated data from the data generation template. A variable "group" is appended indicating group membership.
}
\details{
If the \code{lavaan} script or the \code{MxModel} are provided, the model-implied covariance matrix will be computed and internally use \code{\link{createData}} function to generate data. The data-generation method is based on whether the \code{indDist} argument is specified. For the \code{lavaan} script, the code for data generation is modified from the \code{\link[lavaan]{simulateData}} function.

If the \code{\linkS4class{SimSem}} object is specified, it will check whether there are any random parameters or trivial misspecification in the model. If so, real or misspecified parameters are drawn via the \code{\link{draw}} function. Next, there are two methods to generate data. First, the function will calculate the model-implied covariance matrix (including model misspecification) and generate data similar to the \code{lavaan} script or the \code{MxModel} object. The second method is referred to as the \code{sequential} method, which can be used by specifying the \code{sequential} argument as \code{TRUE}. This function will create data based on the chain of equations in structural equation modeling such that independent variables and errors are generated and added as dependent variables and the dependent variables will be treated as independent variables in the next equation. For example, in the model with factor A and B are independent variables, factor C are dependent variables, factors A and B are generated first. Then, residual in factor C are created and added with factors A and B. This current step has all factor scores. Then, measurement errors are created and added with factor scores to create indicator scores. During each step, independent variables and errors can be nonnormal by setting \code{facDist} or \code{errorDist} arguments. The data generation in each step is based on the \code{\link{createData}} function.

For the model-based bootstrap (providing the \code{realData} argument), the transformation proposed by Yung & Bentler (1996) is used. This procedure is the expansion from the Bollen and Stine (1992) bootstrap including a mean structure. The model-implied mean vector and covariance matrix with trivial misspecification will be used in the model-based bootstrap if \code{misspec} is specified. See page 133 of Bollen and Stine (1992) for a reference. 
}
\references{
Bollen, K. A., & Stine, R. A. (1992). Bootstrapping goodness-of-fit measures in structural equation models. \emph{Sociological Methods and Research, 21,} 205-229.

Yung, Y.-F., & Bentler, P. M. (1996). Bootstrapping techniques in analysis of mean and covariance structures. In G. A. Marcoulides & R. E. Schumacker (Eds.), \emph{Advanced structural equation modeling: Issues and techniques} (pp. 195-226). Mahwah, NJ: Erlbaum.
}
\author{	
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu}), Patrick Miller (Univeristy of Notre Dame; \email{pmille13@nd.edu}), the data generation code for lavaan script is modifed from the \code{simulateData} function in \code{lavaan} written by Yves Rosseel
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

dat <- generate(CFA.Model, 200)

# Get the latent variable scores

dat2 <- generate(CFA.Model, 20, sequential = TRUE, saveLatentVar = TRUE)
dat2
attr(dat2, "latentVar")
}

