# simMissing: A constructor of missing object

# Need container for longitudinal planned missing (e.g., number of items per time)

# Arguments for creating and handling missing data. Currently, package is set to "default" which is FIML by lavaan. Can optionally be set to "Amelia" for MI.
# Arguments for Amelia (including number of imputations) can then be passed via ...
# Later, support for other imputation packages will be included.
miss <- function(cov = 0, pmMCAR = 0, pmMAR = 0, nforms = 0, itemGroups = list(0), timePoints = 1, twoMethod = 0, prAttr = 0, package="default", ignoreCols = 0, threshold = 0, covAsAux = TRUE, logical = NULL, ...) {
  args <- list(...)

  if (!is.null(logical) && is.data.frame(logical)) logical <- as.matrix(logical)
  if(!is.null(logical)) {
    stopifnot(is.logical(logical))
    stopifnot(is.matrix(logical))    
  } else { logical <- logical(0) }
  if(!(package == "default" || package == "Amelia")) { stop("Only \"default\" or \"Amelia\" are accepted as arguments to package.") }
    return(new("SimMissing", cov = cov, pmMCAR = pmMCAR, pmMAR = pmMAR, nforms = nforms, itemGroups = itemGroups, twoMethod = twoMethod, prAttr = prAttr,
               timePoints = timePoints, threshold = threshold, ignoreCols = ignoreCols, covAsAux = covAsAux, logical = logical, package=package, args=args))
} 

#simMissing <- function(cov = 0, pmMCAR = 0, pmMAR = 0, nforms = 0, itemGroups = list(0), timePoints = 1, twoMethod = 0, prAttr = 0, ignoreCols = 0, threshold = 0, covAsAux = TRUE, 
#    logical = FALSE) {
#    if (is(logical, "data.frame")) 
#        logical <- as.matrix(logical)
#    return(new("SimMissing", cov = cov, pmMCAR = pmMCAR, pmMAR = pmMAR, nforms = nforms, itemGroups = itemGroups, twoMethod = twoMethod, prAttr = prAttr, timePoints = timePoints, threshold = threshold, ignoreCols = ignoreCols, covAsAux = covAsAux, logical = logical))
#} 


