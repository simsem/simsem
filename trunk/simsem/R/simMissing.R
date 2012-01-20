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
        numImps="numeric"),
    prototype(
        covs=NULL,
        pmMCAR=NULL,
        pmMAR=NULL,
        nforms=NULL,
        itemGroups=NULL,
        twoMethod=NULL,
        impMethod="amelia",
        numImps=NULL)
)

# simMissing <- function(covs=NULL, pmMCAR=NULL, pmMAR=NULL, nforms=NULL, itemGroups=NULL, twoMethod=NULL) {
#  return(new("simMissing",covs=covs, pmMCAR=pmMCAR, pmMAR=pmMAR, nforms=nforms, itemGroups=itemGroups,
#             twoMethod=twoMethod))}
