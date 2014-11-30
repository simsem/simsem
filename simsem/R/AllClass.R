
# Empty defaults to matrix with dimensions (0,0)
setClass("SimMatrix", representation(free = "matrix", popParam = "matrix", misspec = "matrix", 
    symmetric = "logical"))
# Waiting to put prior

# Empty defaults to vector of length 0
setClass("SimVector", representation(free = "vector", popParam = "vector", misspec = "vector"))
# Waiting to put prior

setClass("SimSem", representation(pt = "list", dgen = "list", modelType = "character", 
    groupLab = "character", con = "list"))

# Set a null class to make sure that the default setting is used in data distribution object
setClass("NullCopula", representation(p = "vector"), prototype(p = 0))
	  
setClass("SimDataDist", representation(p = "numeric", margins = "character", paramMargins = "list", 
    keepScale = "logical", reverse = "vector", copula = "ANY", skewness = "vector", kurtosis = "vector"), prototype(keepScale = TRUE, reverse = FALSE, copula = new("NullCopula"), skewness = NA, kurtosis = NA))

setClass("SimResult", representation(modelType = "character", nRep = "numeric", coef = "data.frame", 
    se = "data.frame", fit = "data.frame", converged = "vector", paramValue = "data.frame", stdParamValue = "data.frame",
    misspecValue = "data.frame", popFit = "data.frame", FMI1 = "data.frame", FMI2 = "data.frame", cilower = "data.frame", ciupper = "data.frame",
    stdCoef = "data.frame", stdSe = "data.frame", seed = "numeric", n = "vector", nobs="data.frame", pmMCAR = "vector", pmMAR = "vector", 
    extraOut = "list", paramOnly = "logical", timing = "list"))

setClass("SimMissing", representation(cov = "vector", pmMCAR = "numeric", pmMAR = "numeric", logit = "character", 
    nforms = "numeric", itemGroups = "list", twoMethod = "vector", prAttr = "vector", m = "numeric", chi = "character", 
    package = "character", convergentCutoff = "numeric", timePoints = "numeric", ignoreCols = "vector", threshold = "numeric", 
    covAsAux = "logical", logical = "matrix", args = "list")) 
