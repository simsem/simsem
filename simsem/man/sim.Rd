\name{sim}
\alias{sim}
\title{
	Run a monte carlo simulation with a structural equation model.
}
\description{
	This function will draw parameters in data-generation model, create data, impose missing, analyze data by the target model, and summarize the outputs across replications. Data can be transformed by the \code{datafun} argument. The additional output can be extracted by the \code{outfun} argument. Paralleled processing is also availabe by the \code{multicore} argument.
}
\usage{
sim(nRep, model, n, generate = NULL, rawData = NULL, miss = NULL, datafun=NULL, outfun=NULL,
pmMCAR = NULL, pmMAR = NULL, facDist = NULL, indDist = NULL, errorDist = NULL, sequential = FALSE, 
modelBoot = FALSE, realData = NULL, maxDraw = 50, misfitType = "f0", 
misfitBounds = NULL, averageNumMisspec = NULL, optMisfit=NULL, optDraws = 50, 
aux = NULL, seed = 123321, silent = FALSE, multicore = FALSE, cluster = FALSE, numProc = NULL,  
paramOnly = FALSE, dataOnly=FALSE, ...)
}
\arguments{
  \item{nRep}{
	Number of replications. Users can specify as \code{NULL} and specify \code{n}, \code{pmMCAR}, and \code{pmMAR}
}
  \item{model}{
	\code{\linkS4class{SimSem}} object created by \code{\link{model}}. Will be used to generate data and analyze it.
}
  \item{n}{
	Sample size. This argument is not necessary except the user wish to vary sample size across replications. The sample size here is a vector of sample size in integers. For the random distribution object, if the resulting value has decimal, the value will be rounded. 
}
  \item{generate}{
	\code{\linkS4class{SimSem}} object created by \code{\link{model}}. If included, this will be used to generate data instead of the code{\linkS4class{SimSem}} object specified in the \code{model} option.
}
  \item{rawData}{
	If included, this data is used for simulations instead of being generated from the \code{SimSem} template. Should be a list of length nRep.
}
  \item{miss}{
	Missing data handling template, created by the function \code{\link{miss}}.
}
  \item{datafun}{
	Function to be applied to generated data set at each replication.
}
  \item{outfun}{
	Function to be applied to the \code{\linkS4class{lavaan}} output at each replication. The output of this function in each replication will be saved in the simulation output (\code{\linkS4class{SimResult}}). The extra outputs can be obtained by the \code{\link{getExtraOutput}} function.
}
  \item{pmMCAR}{
	The percent completely missing at random. This argument is not necessary except the user wish to vary percent missing completely at random across replications. The \code{pmMCAR} here is a vector of percent missing, which the values can be in between 0 and 1 only. The specification of \code{objMissing} is not needed (but is needed if users wish to specify complex missing value data generation or wish to use multiple imputation). 
}
  \item{pmMAR}{
	The percent missing at random. This argument is not necessary except the user wish to vary percent missing at random across replications. The \code{pmMAR} here is a vector of percent missing, which the values can be in between 0 and 1 only. The specification of \code{objMissing} is not needed (but is needed if users wish to specify complex missing value data generation or wish to use multiple imputation). 

}
  \item{facDist}{ 
	A \code{\linkS4class{SimDataDist}} object or list of objects for the distribution of factors. If one object is passed, all factors will have the same distribution. Use when \code{sequential} is \code{TRUE}.
}
  \item{indDist}{ 
	A \code{\linkS4class{SimDataDist}} object or list of objects for a distribution of indicators. If one object is passed, each indicator will have the same distribution. Use when \code{sequential} is \code{FALSE}.
}
  \item{errorDist}{ 
	An object or list of objects of type \code{SimDataDist} indicating the distribution of errors. If a single \code{SimDataDist} is specified, each error will be genrated with that distribution.
}
  \item{sequential}{ 
	If \code{TRUE}, use a sequential method to create data such that the data from factor are generated first and apply to a set of equations to obtain the data of indicators. If \code{FALSE}, create data directly from model-implied mean and covariance of indicators.
}
  \item{modelBoot}{ 
	 When specified, a model-based bootstrap is used for data generation. See \code{\link{draw}} for further information. This argument requires real data to be passed to \code{realData}.
}
  \item{realData}{ 
	A data.frame containing real data. The data generated will follow the distribution of this data set.
}
  \item{maxDraw}{ 
	Integer specifying the maximum number of attempts to draw a valid set of parameters (no negative error variance, standardized coefficients over 1).
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
  \item{aux}{
    The names of auxiliary variables saved in a vector
}
  \item{seed}{ 
	Random number seed. Reproducibility across multiple cores or clusters is ensured using R'Lecuyer package.
}
  \item{silent}{ 
	If \code{TRUE}, suppress warnings.
}
  \item{multicore}{ 
	Use multiple processors within a computer. Specify as TRUE to use it.
}
  \item{cluster}{ 
	Not applicable now. Use for specify nodes in hpc in order to be parallelizable.
}
  \item{numProc}{ 
	Number of processors for using multiple processors. If it is \code{NULL}, the package will find the maximum number of processors.
}
  \item{paramOnly}{ 
	If \code{TRUE}, only the parameters from each replication will be returned.
}
  \item{dataOnly}{ 
	If \code{TRUE}, only the data generated from each replication will be returned.
}
  \item{\dots}{ 
	Additional arguments to be passed to lavaan.
}
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
Output <- sim(5, CFA.Model,n=200)
summary(Output)

# Example of data transformation: Transforming to standard score
fun1 <- function(data) {
	temp <- scale(data)
	temp[,"group"] <- data[,"group"]
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

