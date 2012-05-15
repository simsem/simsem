# validateObject: Validate whether the parameter drawn is good (identified)

validateObject <- function(object) {
    if (!is(object, "MatrixSet")) 
        stop("The object is not a MatrixSet object")
    if (validateCovariance(object@VPS, object@RPS, object@VE) == FALSE) 
        return(FALSE)
    if (object@modelType == "Path" | object@modelType == "Path.exo" | object@modelType == "SEM" | object@modelType == "SEM.exo") {
        if (validatePath(object@BE, object@VE, object@VE) == FALSE) 
            return(FALSE)
    }
    if (object@modelType == "CFA" | object@modelType == "SEM" | object@modelType == "SEM.exo") {
        if (validateCovariance(object@VTE, object@RTE, object@VY) == FALSE) 
            return(FALSE)
        if (validatePath(object@LY, object@VE, object@VY) == FALSE) 
            return(FALSE)
    }
    if (object@modelType == "Path.exo" | object@modelType == "SEM.exo") {
        if (validateCovariance(object@VPH, object@RPH) == FALSE) 
            return(FALSE)
        if (validatePath(object@GA, object@VPH, object@VE) == FALSE) 
            return(FALSE)
    }
    if (object@modelType == "SEM.exo") {
        if (validateCovariance(object@VTD, object@RTD, object@VX) == FALSE) 
            return(FALSE)
        if (validatePath(object@LX, object@VPH, object@VX) == FALSE) 
            return(FALSE)
        maximum.RTH <- sqrt(object@VTD) %o% sqrt(object@VTE)
        abs.RTH <- abs(object@RTH)
        if (sum(abs.RTH > maximum.RTH) > 0) 
            return(FALSE)
    }
    return(TRUE)
} 
