# countFreeParameters: Count how many parameters in each object

setMethod("countFreeParameters", signature = "SimMatrix", definition = function(object) {
    if (isNullObject(object)) {
        return(0)
    } else {
        Labels <- object@free
        return(sum(is.na(Labels)))
    }
})

setMethod("countFreeParameters", signature = "SymMatrix", definition = function(object) {
    if (isNullObject(object)) {
        return(0)
    } else {
        Labels <- object@free
        return(sum(is.na(Labels[upper.tri(Labels, diag = TRUE)])))
    }
})

setMethod("countFreeParameters", signature = "SimVector", definition = function(object) {
    if (isNullObject(object)) {
        return(0)
    } else {
        Labels <- object@free
        return(sum(is.na(Labels)))
    }
})

setMethod("countFreeParameters", signature = "SimSet", definition = function(object) {
    return(sum(c(countFreeParameters(object@LY), countFreeParameters(object@RTE), countFreeParameters(object@VTE), countFreeParameters(object@RPS), countFreeParameters(object@VPS), countFreeParameters(object@BE), 
        countFreeParameters(object@TY), countFreeParameters(object@AL), countFreeParameters(object@ME), countFreeParameters(object@MY), countFreeParameters(object@VE), countFreeParameters(object@VY), 
        countFreeParameters(object@LX), countFreeParameters(object@RTD), countFreeParameters(object@VTD), countFreeParameters(object@RPH), countFreeParameters(object@GA), countFreeParameters(object@TX), 
        countFreeParameters(object@KA), countFreeParameters(object@MX), countFreeParameters(object@VPH), countFreeParameters(object@VX), countFreeParameters(object@RTH))))
})

setMethod("countFreeParameters", signature = "matrix", definition = function(object, symmetric = FALSE) {
    if (symmetric) {
        return(sum(is.na(object[upper.tri(object, diag = TRUE)])))
    } else {
        return(sum(is.na(object)))
    }
})

setMethod("countFreeParameters", signature = "vector", definition = function(object) {
    return(sum(is.na(object)))
})

setMethod("countFreeParameters", signature = "VirtualRSet", definition = function(object) {
    return(sum(c(countFreeParameters(object@LY, symmetric = FALSE), countFreeParameters(object@TE, symmetric = TRUE), countFreeParameters(object@PS, symmetric = TRUE), countFreeParameters(object@BE, 
        symmetric = FALSE), countFreeParameters(object@TY), countFreeParameters(object@AL), countFreeParameters(object@LX, symmetric = FALSE), countFreeParameters(object@TD, symmetric = TRUE), countFreeParameters(object@PH, 
        symmetric = TRUE), countFreeParameters(object@GA, symmetric = FALSE), countFreeParameters(object@TX), countFreeParameters(object@KA), countFreeParameters(object@TH, symmetric = FALSE))))
})

setMethod("countFreeParameters", signature = "SimEqualCon", definition = function(object) {
    con <- object@con
    conElement <- sapply(con, nrow) - 1  # Delete the first element of each constraint
    return(0 - sum(conElement))
})

setMethod("countFreeParameters", signature = "SimREqualCon", definition = function(object) {
    con <- object@con
    conElement <- sapply(con, nrow) - 1  # Delete the first element of each constraint
    return(0 - sum(conElement))
}) 
