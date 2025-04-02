# simMissing: A constructor of missing object

# Need container for longitudinal planned missing (e.g., number of items per
# time)

# Arguments for creating and handling missing data. Currently, package is set
# to 'default' which is FIML by lavaan.

#TDJ 2 April 2025: Deprecated option to impute within analyze()
#TODO:  Use ... to pass arguments to lavaan.mi() --> lavaanList()
#       VIGNETTE IDEA:
#       use more store.slots= or FUN= to enable extra output for sim(outfun=)

miss <- function(cov = 0, pmMCAR = 0, pmMAR = 0, logit = "", nforms = 0,
                 itemGroups = list(), timePoints = 1, twoMethod = 0, prAttr = 0,
                 m = 0, convergentCutoff = 0.8, # package = "default",
                 ignoreCols = 0, threshold = 0,
                 covAsAux = TRUE, logical = NULL, ...) {
  args <- list(...)

  if (!is.null(logical) && is.data.frame(logical)) logical <- as.matrix(logical)
  if (!is.null(logical)) {
      stopifnot(is.logical(logical))
      stopifnot(is.matrix(logical))
  } else {
      logical <- as.matrix(FALSE)
  }
  ##FIXME: without runMI(), no sustainable way to automate imputation
#   if (!(package %in% c("default", "Amelia", "mice"))) {
#       stop("Only \"default\", \"mice\", or \"Amelia\" are accepted as arguments to package.")
#   }
# 	if (package %in% c("Amelia", "mice")) {
# 		if (!(m > 0)) stop("Because the Amelia or mice packages are specified,",
# 		                   " the m argument should be specified.")
# 	}
# 	if (package == "default") {
# 		if (m > 0) package <- "mice"
# 	}
  return(new("SimMissing", cov = cov, pmMCAR = pmMCAR, pmMAR = pmMAR,
             logit = logit, nforms = nforms, itemGroups = itemGroups,
             twoMethod = twoMethod, prAttr = prAttr, timePoints = timePoints,
             threshold = threshold, ignoreCols = ignoreCols,
             covAsAux = covAsAux, logical = logical,
             #FIXME: reinstate m > 0, but remove package= from SimMissing-class
             m = 0, package = "default", convergentCutoff = convergentCutoff,
             args = args))
}
