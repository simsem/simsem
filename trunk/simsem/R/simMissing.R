#Need container for longitudinal planned missing (e.g., number of items per time)
setClass("simMissing",
    representation(
        covs="vector",
        pmMCAR="numeric",
        pmMAR="numeric",
        nforms="numeric",
        itemGroups="list",
        twoMethod="vector",
        impMethod="vector",
        numImps="numeric",
        timePoints="numeric"),
    prototype(
        covs=0,
        pmMCAR=0,
        pmMAR=0,
        nforms=0,
        itemGroups=list(0),
        twoMethod=0,
        impMethod="amelia",
        numImps=0,
        timePoints=1)
)

# simMissing <- function(covs=NULL, pmMCAR=NULL, pmMAR=NULL, nforms=NULL, itemGroups=NULL, twoMethod=NULL) {
#  return(new("simMissing",covs=covs, pmMCAR=pmMCAR, pmMAR=pmMAR, nforms=nforms, itemGroups=itemGroups,
#             twoMethod=twoMethod))}
