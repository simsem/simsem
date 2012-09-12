# divideObject: Make a division on each elements of the object

setMethod("divideObject", signature(object = "vector", constant = "numeric"), definition = function(object, constant) {
    if (isNullObject(object)) {
        return(new("NullVector"))
    } else {
        return(object/constant)
    }
})

setMethod("divideObject", signature(object = "matrix", constant = "numeric"), definition = function(object, constant, correlation = FALSE) {
    if (isNullObject(object)) {
        return(new("NullMatrix"))
    } else {
        if (correlation) {
            temp <- diag(object)
            object <- object/constant
            diag(object) <- temp
            return(object)
        } else {
            return(object/constant)
        }
    }
})

setMethod("divideObject", signature(object = "MatrixSet", constant = "numeric"), definition = function(object, constant) {
    LY <- divideObject(object@LY, constant)
    VTE <- divideObject(object@VTE, constant)
    RTE <- divideObject(object@RTE, constant, correlation = TRUE)
    VY <- divideObject(object@VY, constant)
    TY <- divideObject(object@TY, constant)
    MY <- divideObject(object@MY, constant)
    BE <- divideObject(object@BE, constant)
    VPS <- divideObject(object@VPS, constant)
    RPS <- divideObject(object@RPS, constant, correlation = TRUE)
    VE <- divideObject(object@VE, constant)
    AL <- divideObject(object@AL, constant)
    ME <- divideObject(object@ME, constant)
    LX <- divideObject(object@LX, constant)
    VTD <- divideObject(object@VTD, constant)
    RTD <- divideObject(object@RTD, constant, correlation = TRUE)
    VX <- divideObject(object@VX, constant)
    TX <- divideObject(object@TX, constant)
    MX <- divideObject(object@MX, constant)
    GA <- divideObject(object@GA, constant)
    VPH <- divideObject(object@VPH, constant)
    RPH <- divideObject(object@RPH, constant, correlation = TRUE)
    KA <- divideObject(object@KA, constant)
    RTH <- divideObject(object@RTH, constant)
    Output <- new("MatrixSet", modelType = object@modelType, LY = LY, VTE = VTE, RTE = RTE, VY = VY, TY = TY, MY = MY, BE = BE, VPS = VPS, 
        RPS = RPS, VE = VE, AL = AL, ME = ME, LX = LX, VTD = VTD, RTD = RTD, VX = VX, TX = TX, MX = MX, GA = GA, VPH = VPH, RPH = RPH, 
        KA = KA, RTH = RTH)
    return(Output)
}) 
