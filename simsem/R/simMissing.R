# simMissing: A constructor of missing object

# Need container for longitudinal planned missing (e.g., number of items per time)

simMissing <- function(cov = 0, pmMCAR = 0, pmMAR = 0, nforms = 0, itemGroups = list(0), timePoints = 1, twoMethod = 0, prAttr = 0, 
    numImps = 0, ignoreCols = 0, threshold = 0, covAsAux = TRUE, logical = new("NullMatrix"), ...) {
    args <- list(...)
    if (is(logical, "data.frame")) 
        logical <- as.matrix(logical)
    return(new("SimMissing", cov = cov, pmMCAR = pmMCAR, pmMAR = pmMAR, nforms = nforms, itemGroups = itemGroups, twoMethod = twoMethod, 
        prAttr = prAttr, timePoints = timePoints, threshold = threshold, ignoreCols = ignoreCols, numImps = numImps, covAsAux = covAsAux, logical = logical, opts = args))
} 
