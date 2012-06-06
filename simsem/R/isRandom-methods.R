# isRandom: Check whether the object has random object

setMethod("isRandom", signature(object = "SimMatrix"), definition = function(object) {
    if (isNullObject(object)) {
        return(FALSE)
    } else {
        obj1 <- run(object)
        obj2 <- run(object)
        compare <- all.equal(obj1, obj2)
        if ((length(compare) == 1) && (compare == TRUE)) {
            return(FALSE)
        } else {
            return(TRUE)
        }
    }
})

setMethod("isRandom", signature(object = "SimVector"), definition = function(object) {
    if (isNullObject(object)) {
        return(FALSE)
    } else {
        obj1 <- run(object)
        obj2 <- run(object)
        compare <- all.equal(obj1, obj2)
        if ((length(compare) == 1) && (compare == TRUE)) {
            return(FALSE)
        } else {
            return(TRUE)
        }
    }
})

setMethod("isRandom", signature = "SimSet", definition = function(object) {
    return(any(isRandom(object@LY), isRandom(object@RTE), isRandom(object@VTE), isRandom(object@RPS), isRandom(object@VPS), isRandom(object@BE), 
        isRandom(object@TY), isRandom(object@AL), isRandom(object@ME), isRandom(object@MY), isRandom(object@VE), isRandom(object@VY), 
        isRandom(object@LX), isRandom(object@RTD), isRandom(object@VTD), isRandom(object@RPH), isRandom(object@GA), isRandom(object@TX), 
        isRandom(object@KA), isRandom(object@MX), isRandom(object@VPH), isRandom(object@VX), isRandom(object@RTH)))
})

setMethod("isRandom", signature = "SimMisspec", definition = function(object) {
    if (isNullObject(object)) {
        return(FALSE)
    } else {
        if (object@optMisfit != "none") {
            return(FALSE)
        } else {
            return(any(isRandom(object@LY), isRandom(object@RTE), isRandom(object@VTE), isRandom(object@RPS), isRandom(object@VPS), isRandom(object@BE), 
                isRandom(object@TY), isRandom(object@AL), isRandom(object@ME), isRandom(object@MY), isRandom(object@VE), isRandom(object@VY), 
                isRandom(object@LX), isRandom(object@RTD), isRandom(object@VTD), isRandom(object@RPH), isRandom(object@GA), isRandom(object@TX), 
                isRandom(object@KA), isRandom(object@MX), isRandom(object@VPH), isRandom(object@VX), isRandom(object@RTH)))
        }
    }
})

setMethod("isRandom", signature = "SimData", definition = function(object) {
    isRandom(object@param) || isRandom(object@misspec)
})
 
