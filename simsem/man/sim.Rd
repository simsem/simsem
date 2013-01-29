\name{sim}
\alias{sim}
\title{
	Run a monte carlo simulation with a structural equation model.
}
\description{
	This function can be used to generate data, analyze the generated data, and summarized into a result object where parameter estimates, standard errors, fit indices, and other characteristics of each replications are saved. The data-generation and data analysis can be done by 1. \code{\linkS4class{SimSem}} objects created with the \code{\link{model}} function, 2. \code{lavaan} script or parameter tables, or 3. an \code{MxModel} object from the \code{OpenMx} package. 
}
\usage{
sim(nRep, model, n, generate = NULL, rawData = NULL, miss = NULL, datafun=NULL, 
lavaanfun = "lavaan", outfun=NULL, pmMCAR = NULL, pmMAR = NULL, facDist = NULL, indDist = NULL, 
errorDist = NULL, sequential = FALSE, modelBoot = FALSE, realData = NULL, 
covData = NULL, maxDraw = 50, misfitType = "f0", misfitBounds = NULL, 
averageNumMisspec = FALSE, optMisfit=NULL, optDraws = 50, 
createOrder = c(1, 2, 3), aux = NULL, group = NULL, mxFit = FALSE,
seed = 123321, silent = FALSE, 
multicore = FALSE, cluster = FALSE, numProc = NULL, paramOnly = FALSE, 
dataOnly=FALSE, smartStart=FALSE, previousSim = NULL, completeRep = FALSE,  ...)
}
\arguments{
  \item{nRep}{
	Number of replications. If any of the \code{n}, \code{pmMCAR}, or \code{pmMAR} arguments are specified as lists, the number of replications will default to the length of the list(s), and \code{nRep} need not be specified.
}
  \item{model}{
	There are three options for this argument: 1. \code{\linkS4class{SimSem}} object created by \code{\link{model}}, 2. \code{lavaan} script or \code{lavaan} parameter table, or 3. a list that contains all argument that users use to run \code{lavaan} (including \code{cfa}, \code{sem}, \code{lavaan}). For the \code{\linkS4class{SimSem}} object, if the \code{generate} argument is not specified, then the object in the \code{model} argument will be used for both data generation and analysis. If \code{generate} is specified, then the \code{model} argument will be used for data analysis only. 
}
  \item{n}{
	Sample size. Either a single value, or a list of values to vary sample size across replications. The \code{n} argument can also be specified as a random distribution object; if any resulting values are non-integers, the decimal will be rounded. 
}
  \item{generate}{
	There are three options for this argument: 1. \code{\linkS4class{SimSem}} object created by \code{\link{model}}, 2. \code{lavaan} script or \code{lavaan} parameter table (for data generation; see \code{\link[lavaan]{simulateData}}), or 3. a list that contains all argument that users use to run \code{\link[lavaan]{simulateData}}. This argument cannot be specified the same time as the \code{rawData} argument
}
  \item{rawData}{
	There are two options for this argument: 1. a list of data frames to be used in simulations or 2. a population data. If a list of data frames is specified, the \code{nRep} and \code{n} arguments must not be specified. If a population data frame is specified, the \code{nRep} and \code{n} arguments are required.
}
  \item{miss}{
	A missing data template created using the \code{\link{miss}} function.
}
  \item{datafun}{
	A function to be applied to each generated data set across replications.
}
  \item{lavaanfun}{
	The character of the function name used in running lavaan model (\code{"cfa"}, \code{"sem"}, \code{"growth"}, \code{"lavaan"}). This argument is required only when lavaan script or a list of arguments is specified in the \code{model} argument.
}
  \item{outfun}{
	A function to be applied to the \code{\linkS4class{lavaan}} output at each replication. Output from this function in each replication will be saved in the simulation output (\code{\linkS4class{SimResult}}), and can be obtained using the \code{\link{getExtraOutput}} function.
}
  \item{pmMCAR}{
	The percentage of data completely missing at random (0 <= pmMCAR < 1). Either a single value or a vector of values in order to vary pmMCAR across replications (with length equal to nRep or a divisor of nRep). The \code{objMissing} argument is only required when specifying complex missing value data generation, or when using multiple imputation. 
}
  \item{pmMAR}{
	The percentage of data missing at random (0 <= pmCAR < 1). Either a single value or a vector of values in order to vary pmCAR across replications (with length equal to nRep or a divisor of nRep). The \code{objMissing} argument is only required when specifying complex missing value data generation, or when using multiple imputation. 

}
  \item{facDist}{ 
	Factor distributions. Either a list of \code{\linkS4class{SimDataDist}} objects or a single \code{\linkS4class{SimDataDist}} object to give all factors the same distribution. Use when \code{sequential} is \code{TRUE}.
}
  \item{indDist}{ 
	Indicator distributions. Either a list of \code{\linkS4class{SimDataDist}} objects or a single \code{\linkS4class{SimDataDist}} object to give all indicators the same distribution. Use when \code{sequential} is \code{FALSE}.
}
  \item{errorDist}{ 
	An object or list of objects of type \code{SimDataDist} indicating the distribution of errors. If a single \code{SimDataDist} is specified, each error will be genrated with that distribution.
}
  \item{sequential}{ 
	If \code{TRUE}, a sequential method is used to generate data in which factor data is generated first, and is subsequently applied to a set of equations to obtain the indicator data. If \code{FALSE}, data is generated directly from model-implied mean and covariance of the indicators.
}
  \item{modelBoot}{ 
	 When specified, a model-based bootstrap is used for data generation (for use with the \code{realData} argument). See \code{\link{draw}} for further information.
}
  \item{realData}{ 
	A data.frame containing real data. Generated data will follow the distribution of this data set.
}
  \item{covData}{ 
	A data.frame containing covariate data, which can have any distributions. This argument is required when users specify \code{GA} or \code{KA} matrices in the model template (\code{\linkS4class{SimSem}}).
}
  \item{maxDraw}{ 
	The maximum number of attempts to draw a valid set of parameters (no negative error variance, standardized coefficients over 1).
}
  \item{misfitType}{ 
	Character vector indicating the fit measure used to assess the misfit of a set of parameters. Can be "f0", "rmsea", "srmr", or "all". 
}
  \item{misfitBounds}{ 
	Vector that contains upper and lower bounds of the misfit measure. Sets of parameters drawn that are not within these bounds are rejected.
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
  \item{aux}{
    The names of auxiliary variables saved in a vector.
}
  \item{group}{
    The name of the group variable. This argument is used when \code{lavaan} script or \code{MxModel} is used in the \code{model} only.
}
  \item{mxFit}{
    A logical whether to find an extensive list of fit measures (which will be slower). This argument is applicable when \code{MxModel} is used in the \code{model} argument only.
}
  \item{seed}{ 
	Random number seed. Reproducibility across multiple cores or clusters is ensured using R'Lecuyer package.
}
  \item{silent}{ 
	If \code{TRUE}, suppress warnings.
}
  \item{multicore}{ 
	If \code{TRUE}, multiple processors within a computer will be utilized.
}
  \item{cluster}{ 
	Not applicable now. Used to specify nodes in hpc in order to be parallelizable.
}
  \item{numProc}{ 
	Number of processors for using multiple processors. If it is \code{NULL}, the package will find the maximum number of processors.
}
  \item{paramOnly}{ 
	If \code{TRUE}, only the parameters from each replication will be returned.
}
  \item{dataOnly}{ 
	If \code{TRUE}, only the raw data generated from each replication will be returned.
}
  \item{smartStart}{
	Defaults to FALSE. If TRUE, population parameter values that are real numbers will be used as starting values. When tested in small models, the time elapsed when using population values as starting values was greater than the time reduced during analysis, and convergence rates were not affected. 
}
  \item{previousSim}{
	A result object that users wish to add the results of the current simulation in
}
  \item{completeRep}{
	If \code{TRUE}, the function will run until the number of convergent replication equal to the specified \code{nRep}.
}
  \item{\dots}{ 
	Additional arguments to be passed to lavaan.
}
}
\details{
This function is executed as follows: 1. parameters are drawn from the specified data-generation model (applicable only simsem model template, \code{\linkS4class{SimSem}}, only), 2. the drawn (or the specified) parameters are used to create data, 3. data can be transformed using the \code{datafun} argument, 4. specified missingness (if any) is imposed, 5. data are analyzed using the specified analysis model, 6. parameter estimates, standard errors, fit indices, and other characteristics of a replication are extracted, 7. additional outputs (if any) are extracted using the \code{outfun} argument, and 8. results across replications are summarized in a result object, \code{\linkS4class{SimResult}}).

The \code{sim} function can also be used to obtain raw data using the \code{dataOnly} argument, to analyze pre-existing data or a drawn of a specified population data using the \code{rawData} argument, and to simulate data that follows the distribution of a real data set using the \code{realData} argument. Paralleled processing can be enabled using the \code{multicore} argument. 
}
\value{
	A result object (\code{\linkS4class{SimResult}})
}
\author{
	Patrick Miller (Univeristy of Notre Dame; \email{pmille13@nd.edu})
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimResult}} for the resulting output description
	}
}
\examples{
# Please go to www.simsem.org for more examples.

# Example of using simsem model template

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

# In reality, more than 5 replications are needed.
Output <- sim(5, CFA.Model, n=200)
summary(Output)

# Example of using simsem model template

popModel <- "
f1 =~ 0.7*y1 + 0.7*y2 + 0.7*y3
f2 =~ 0.7*y4 + 0.7*y5 + 0.7*y6
f1 ~~ 1*f1
f2 ~~ 1*f2
f1 ~~ 0.5*f2
y1 ~~ 0.49*y1
y2 ~~ 0.49*y2
y3 ~~ 0.49*y3
y4 ~~ 0.49*y4
y5 ~~ 0.49*y5
y6 ~~ 0.49*y6
"

analysisModel <- "
f1 =~ y1 + y2 + y3
f2 =~ y4 + y5 + y6
"

Output <- sim(5, model=analysisModel, n=200, generate=popModel, std.lv=TRUE, lavaanfun = "cfa")
summary(Output)

# Example of using population data

pop <- data.frame(y1 = rnorm(100000, 0, 1), y2 = rnorm(100000, 0, 1))

covModel <- "
y1 ~~ y2
"

Output <- sim(5, model=covModel, n=200, rawData=pop, lavaanfun = "cfa")
summary(Output)

# Example of data transformation: Transforming to standard score
fun1 <- function(data) {
	temp <- scale(data)
	as.data.frame(temp)
}

# Example of additional output: Extract modification indices from lavaan
fun2 <- function(out) {
	inspect(out, "mi")
}

# In reality, more than 5 replications are needed.
Output <- sim(5, CFA.Model,n=200,datafun=fun1, outfun=fun2)
summary(Output)

# Get modification indices
getExtraOutput(Output)
}

