# setOpenMxObject: Rearrange starting values such that it is appropriate for OpenMx matrix specification such that free parameters are set to be TRUE/FALSE and values meaning be both fixed
# value or starting values

setMethod("setOpenMxObject", signature(param = "vector", start = "vector"), definition = function(param, start) {
    if (isNullObject(param)) {
        return(start)
    } else {
        start[!is.na(param)] <- param[!is.na(param)]
        return(round(start, 3))
    }
})

setMethod("setOpenMxObject", signature(param = "matrix", start = "matrix"), definition = function(param, start) {
    if (isNullObject(param)) {
        return(start)
    } else {
        start[!is.na(param)] <- param[!is.na(param)]
        return(round(start, 3))
    }
})

setMethod("setOpenMxObject", signature(param = "SimParam", start = "SimRSet"), definition = function(param, start) {
    start@LY <- setOpenMxObject(param@LY, start@LY)
    start@TE <- setOpenMxObject(param@TE, start@TE)
    start@PS <- setOpenMxObject(param@PS, start@PS)
    start@BE <- setOpenMxObject(param@BE, start@BE)
    start@TY <- setOpenMxObject(param@TY, start@TY)
    start@AL <- setOpenMxObject(param@AL, start@AL)
    start@LX <- setOpenMxObject(param@LX, start@LX)
    start@TD <- setOpenMxObject(param@TD, start@TD)
    start@PH <- setOpenMxObject(param@PH, start@PH)
    start@GA <- setOpenMxObject(param@GA, start@GA)
    start@TX <- setOpenMxObject(param@TX, start@TX)
    start@KA <- setOpenMxObject(param@KA, start@KA)
    start@TH <- setOpenMxObject(param@TH, start@TH)
    return(start)
}) 
