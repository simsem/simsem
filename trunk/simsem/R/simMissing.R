#Need container for longitudinal planned missing (e.g., number of items per time)

simMissing <- function(covs=0, pmMCAR=0, pmMAR=0, nforms=0, itemGroups=list(0), timePoints=1, twoMethod=0, numImps=0) {
  return(new("SimMissing",covs=covs, pmMCAR=pmMCAR, pmMAR=pmMAR, nforms=nforms, itemGroups=itemGroups,
             twoMethod=twoMethod, timePoints=timePoints, numImps=numImps))
}
