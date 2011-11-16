setClass("SimMissing",
    representation(
        covs="vector",
        pmMCAR="numeric",
        pmMAR="numeric",
        nforms="numeric",
        itemGroups="list",
        twoMethod="vector"),

    prototype(
        covs=new("NullVector"),
        pmMCAR=0,
        pmMAR=0,
        nforms=0,
        itemGroups=list(),
        twoMethod=new("NullVector"))

)
