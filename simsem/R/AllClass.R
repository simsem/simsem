### Sunthud Pornprasertmanit; with contributions by Terrence D. Jorgensen
### Last updated: 5 March 2026
### source code for S4 class definitions used in simsem

#' Matrix object: Random parameters matrix
#'
#' This object can be used to represent a matrix in an SEM model. It contains free
#' parameters, fixed values, starting values, and model misspecification.
#' This object can represent mean, intercept, or variance matrices.
#'
#' @section Objects from the Class:
#' This object is created by \code{\link{bind}} or \code{\link{binds}} function.
#'
#' @slot free The free-parameter vector. Any NA elements or character elements
#' are free. Any numeric elements are fixed as the specified number. If any free
#' elements have the same characters (except NA), the elements are equally constrained.
#'
#' @slot popParam Population parameter values.
#'
#' @slot misspec Model misspecification values.
#'
#' @slot symmetric Logical value indicating whether the matrix is symmetric.
#'
#' @aliases SimMatrix-class summaryShort,SimMatrix-method summary,SimMatrix-method
#'
#' @docType class
#' @name SimMatrix-class
NULL

### Empty defaults to matrix with dimensions (0,0)
setClass(
  "SimMatrix",
  representation(
    free = "matrix", popParam = "matrix",
    misspec = "matrix", symmetric = "logical"
  )
)

#' Vector object: Random parameters vector
#'
#' This object can be used to represent a vector in an SEM model. It contains free
#' parameters, fixed values, starting values, and model misspecification.
#' This object can represent mean, intercept, or variance vectors.
#'
#' @section Objects from the Class:
#' This object is created by \code{\link{bind}} function.
#'
#' @slot free The free-parameter vector. Any NA elements or character elements
#' are free. Any numeric elements are fixed as the specified number. If any free
#' elements have the same characters (except NA), the elements are equally constrained.
#'
#' @slot popParam Population parameter values.
#'
#' @slot misspec Model misspecification values.
#'
#' @aliases SimVector-class summaryShort,SimVector-method summary,SimVector-method
#'
#' @docType class
#' @name SimVector-class
NULL

### Empty defaults to vector of length 0
setClass(
  "SimVector",
  representation(free = "vector", popParam = "vector", misspec = "vector")
)

#' Class "SimSem"
#'
#' The template containing data-generation and data-analysis specification.
#'
#' @section Objects from the Class:
#' Objects can be created by \code{\link{model}}.
#'
#' @slot pt Parameter table used in data analysis.
#'
#' @slot dgen Data generation template.
#'
#' @slot modelType Type of model (CFA, Path, or SEM) contained in this object.
#'
#' @slot groupLab The label of grouping variable.
#'
#' @slot con The list of defined parameters, equality constraints, or inequality
#' constraints specified in the model.
#'
#' @aliases SimSem-class summary,SimSem-method
#'
#' @docType class
#' @name SimSem-class
NULL

setClass(
  "SimSem",
  representation(
    pt = "list", dgen = "list", modelType = "character",
    groupLab = "character", con = "list"
  )
)

# Set a null class to make sure that the default setting is used in data distribution object
setClass(
  "NullCopula",
  representation(p = "vector"), prototype(p = 0)
)

#' Class "SimDataDist": Data distribution object
#'
#' This class will provide the distribution of a dataset.
#'
#' @section Objects from the Class:
#' Objects can be created by \code{\link{bindDist}} function. It can also be
#' called from the form \code{new("SimDataDist", ...)}.
#'
#' @slot p Number of variables.
#'
#' @slot margins A character vector specifying all the marginal distributions.
#'
#' @slot paramMargins A list whose each component is a list of named components,
#' giving the parameter values of the marginal distributions.
#'
#' @slot keepScale Logical value indicating whether to transform the data to
#' maintain the scale.
#'
#' @slot reverse Vector indicating whether variables should be reversed.
#'
#' @slot copula Copula object specifying the dependence structure.
#'
#' @slot skewness Vector of skewness values.
#'
#' @slot kurtosis Vector of kurtosis values.
#'
#' @aliases SimDataDist-class summary,SimDataDist-method plotDist,SimDataDist-method
#'
#' @docType class
#' @name SimDataDist-class
NULL

setClass(
  "SimDataDist",
  representation(
    p = "numeric", margins = "character", paramMargins = "list",
    keepScale = "logical", reverse = "vector", copula = "ANY",
    skewness = "vector", kurtosis = "vector"
  ),
  prototype(
    keepScale = TRUE, reverse = FALSE, copula = new("NullCopula"),
    skewness = NA, kurtosis = NA
  )
)

#' Class "SimResult": Simulation Result Object
#'
#' This class will save data analysis results from multiple replications, such
#' as fit indices cutoffs or power, parameter values, model misspecification,
#' etc.
#'
#' @section Objects from the Class:
#' Objects can be created by \code{\link{sim}}.
#'
#' @slot modelType Analysis model type (CFA, Path, or SEM).
#'
#' @slot nRep Total number of replications specified in the simulation design
#'
#' @slot coef Parameter estimates from each replication.
#'
#' @slot se Standard errors of parameter estimates from each replication.
#'
#' @slot fit Fit indices from each replication.
#'
#' @slot converged Logical vector indicating convergence status.
#'
#' @slot paramValue Population parameter values.
#'
#' @slot stdParamValue Standardized population parameter values.
#'
#' @slot misspecValue Model misspecification values.
#'
#' @slot popFit Population fit indices.
#'
#' @slot FMI1 Fraction of missing information measure 1.
#'
#' @slot FMI2 Fraction of missing information measure 2.
#'
#' @slot cilower Lower confidence interval bounds.
#'
#' @slot ciupper Upper confidence interval bounds.
#'
#' @slot stdCoef Standardized coefficients.
#'
#' @slot stdSe Standardized standard errors.
#'
#' @slot seed Random seed used in simulation.
#'
#' @slot n Sample size.
#'
#' @slot nobs Number of observed cases.
#'
#' @slot pmMCAR Percent missing completely at random.
#'
#' @slot pmMAR Percent missing at random.
#'
#' @slot extraOut Additional outputs stored in a list.
#'
#' @slot paramOnly Logical indicating whether only parameters are stored.
#'
#' @slot timing Timing information for simulation runs.
#'
#' @slot repRun Integer vector indicating the indices of replications included in this object.
#'
#' @aliases SimResult-class summary,SimResult-method summaryShort,SimResult-method
#'
#' @docType class
#' @name SimResult-class
NULL

setClass(
  "SimResult",
  representation(
    modelType = "character", nRep = "numeric",
    coef = "data.frame", se = "data.frame",
    fit = "data.frame", converged = "vector",
    paramValue = "data.frame", stdParamValue = "data.frame",
    misspecValue = "data.frame", popFit = "data.frame",
    FMI1 = "data.frame", FMI2 = "data.frame",
    cilower = "data.frame", ciupper = "data.frame",
    stdCoef = "data.frame", stdSe = "data.frame",
    seed = "numeric", n = "vector", nobs = "data.frame",
    pmMCAR = "vector", pmMAR = "vector", extraOut = "list",
    paramOnly = "logical", timing = "list", repRun = "integer"
  )
)

#' Class "SimMissing"
#'
#' Missing information imposing on the complete dataset.
#'
#' @section Objects from the Class:
#' Objects can be created by \code{\link{miss}} function.
#'
#' @slot cov Column indices of any normally distributed covariates used in the data set.
#'
#' @slot logit The script used for imposing missing values by logistic regression.
#' See \code{\link{miss}} for further details.
#'
#' @slot pmMCAR Proportion of missingness to introduce completely at random across all variables.
#'
#' @slot pmMAR Proportion of missingness to introduce using the listed covariates as predictors.
#'
#' @slot nforms Number of forms for planned missing designs.
#'
#' @slot itemGroups List specifying item groups.
#'
#' @slot twoMethod Vector specifying two-method missingness.
#'
#' @slot prAttr Vector of probabilities of attrition.
#'
#' @slot m Number of imputations.
#'
#' @slot package Package used for missing data handling.
#'
#' @slot args Additional arguments passed to missing data functions.
#'
#' @slot convergentCutoff Convergence cutoff value.
#'
#' @slot timePoints Time points for longitudinal missingness.
#'
#' @slot ignoreCols Columns to ignore.
#'
#' @slot threshold Threshold values.
#'
#' @slot covAsAux Logical indicating whether covariates are treated as auxiliary variables.
#'
#' @slot logical Logical matrix specifying missing data patterns.
#'
#' @aliases SimMissing-class summary,SimMissing-method
#'
#' @docType class
#' @name SimMissing-class
NULL

setClass(
  "SimMissing",
  representation(
    cov = "vector", logit = "character",
    pmMCAR = "numeric", pmMAR = "numeric",
    nforms = "numeric", itemGroups = "list",
    twoMethod = "vector", prAttr = "vector",
    m = "numeric", package = "character", args = "list",
    convergentCutoff = "numeric", timePoints = "numeric",
    ignoreCols = "vector", threshold = "numeric",
    covAsAux = "logical", logical = "matrix"
  )
)
