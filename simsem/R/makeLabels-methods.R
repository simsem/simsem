# makeLabels: Make parameter names for each element in matrices or vectors for analysis in OpenMx (or other possible packages)

setMethod("makeLabels", signature = "vector", definition = function(object, name, package) {
    if (isNullObject(object)) {
        return(new("NullVector"))
    } else {
        Length <- length(object)
        if (package == "OpenMx") {
            for (i in 1:Length) {
                ifelse(is.na(object[i]), object[i] <- paste(name, i, sep = ""), object[i] <- NA)
            }
            return(object)
        } else if (package == "lavaan") {
            object[] <- ""
            return(object)
        }
    }
})

setMethod("makeLabels", signature = "matrix", definition = function(object, name, package, symmetric = FALSE) {
    if (isNullObject(object)) {
        return(new("NullMatrix"))
    } else {
        np <- nrow(object)
        nq <- ncol(object)
        if (package == "OpenMx") {
            if (symmetric) {
                for (i in 1:np) {
                  for (j in 1:i) {
                    if (is.na(object[i, j])) {
                      object[i, j] <- paste(name, i, "_", j, sep = "")
                    } else {
                      object[i, j] <- NA
                    }
                    if (i != j) 
                      object[j, i] <- object[i, j]
                  }
                }
            } else {
                for (i in 1:np) {
                  for (j in 1:nq) {
                    if (is.na(object[i, j])) {
                      object[i, j] <- paste(name, i, "_", j, sep = "")
                    } else {
                      object[i, j] <- NA
                    }
                  }
                }
            }
        } else if (package == "lavaan") {
            object[, ] <- ""
        }
        return(object)
    }
})

setMethod("makeLabels", signature = "SimParam", definition = function(object, package) {
    LY <- makeLabels(object@LY, "LY", package)
    TE <- makeLabels(object@TE, "TE", package, symmetric = TRUE)
    PS <- makeLabels(object@PS, "PS", package, symmetric = TRUE)
    BE <- makeLabels(object@BE, "BE", package)
    TY <- makeLabels(object@TY, "TY", package)
    AL <- makeLabels(object@AL, "AL", package)
    LX <- makeLabels(object@LX, "LX", package)
    TD <- makeLabels(object@TD, "TD", package, symmetric = TRUE)
    PH <- makeLabels(object@PH, "PH", package, symmetric = TRUE)
    GA <- makeLabels(object@GA, "GA", package)
    TX <- makeLabels(object@TX, "TX", package)
    KA <- makeLabels(object@KA, "KA", package)
    TH <- makeLabels(object@TH, "TH", package)
    return(new("SimLabels", LY = LY, TE = TE, BE = BE, PS = PS, AL = AL, TY = TY, LX = LX, TD = TD, TX = TX, GA = GA, PH = PH, KA = KA, 
        TH = TH, modelType = object@modelType))
})

setMethod("makeLabels", signature = "VirtualDist", definition = function(object, digit = 3) {
    indivAttr <- slotNames(object)
    val <- sapply(indivAttr, slot, object = object)
    val <- round(val, digit)
    lab <- mapply(paste, indivAttr, "=", val)
    lab <- paste(lab, collapse = ", ")
    return(lab)
})

setMethod("makeLabels", signature = "SimSet", definition = function(object, package) {
    ifelse(!isNullObject(object@LY), LY <- makeLabels(object@LY@free, "LY", package), LY <- new("NullMatrix"))
    ifelse(!isNullObject(object@TE), TE <- makeLabels(object@TE@free, "TE", package, symmetric = TRUE), TE <- new("NullMatrix"))
    ifelse(!isNullObject(object@RTE), RTE <- makeLabels(object@RTE@free, "RTE", package, symmetric = TRUE), RTE <- new("NullMatrix"))
    ifelse(!isNullObject(object@VTE), VTE <- makeLabels(object@VTE@free, "VTE", package), VTE <- new("NullVector"))
    ifelse(!isNullObject(object@VY), VY <- makeLabels(object@VY@free, "VY", package), VY <- new("NullVector"))
    ifelse(!isNullObject(object@PS), PS <- makeLabels(object@PS@free, "PS", package, symmetric = TRUE), PS <- new("NullMatrix"))
    ifelse(!isNullObject(object@RPS), RPS <- makeLabels(object@RPS@free, "RPS", package, symmetric = TRUE), RPS <- new("NullMatrix"))
    ifelse(!isNullObject(object@VPS), VPS <- makeLabels(object@VPS@free, "VPS", package), VPS <- new("NullVector"))
    ifelse(!isNullObject(object@VE), VE <- makeLabels(object@VE@free, "VE", package), VE <- new("NullVector"))
    ifelse(!isNullObject(object@BE), BE <- makeLabels(object@BE@free, "BE", package), BE <- new("NullMatrix"))
    ifelse(!isNullObject(object@TY), TY <- makeLabels(object@TY@free, "TY", package), TY <- new("NullVector"))
    ifelse(!isNullObject(object@MY), MY <- makeLabels(object@MY@free, "MY", package), MY <- new("NullVector"))
    ifelse(!isNullObject(object@AL), AL <- makeLabels(object@AL@free, "AL", package), AL <- new("NullVector"))
    ifelse(!isNullObject(object@ME), ME <- makeLabels(object@ME@free, "ME", package), ME <- new("NullVector"))
    ifelse(!isNullObject(object@LX), LX <- makeLabels(object@LX@free, "LX", package), LX <- new("NullMatrix"))
    ifelse(!isNullObject(object@TD), TD <- makeLabels(object@TD@free, "TD", package, symmetric = TRUE), TD <- new("NullMatrix"))
    ifelse(!isNullObject(object@RTD), RTD <- makeLabels(object@RTD@free, "RTD", package, symmetric = TRUE), RTD <- new("NullMatrix"))
    ifelse(!isNullObject(object@VTD), VTD <- makeLabels(object@VTD@free, "VTD", package), VTD <- new("NullVector"))
    ifelse(!isNullObject(object@VX), VX <- makeLabels(object@VX@free, "VX", package), VX <- new("NullVector"))
    ifelse(!isNullObject(object@PH), PH <- makeLabels(object@PH@free, "PH", package, symmetric = TRUE), PH <- new("NullMatrix"))
    ifelse(!isNullObject(object@RPH), RPH <- makeLabels(object@RPH@free, "RPH", package, symmetric = TRUE), RPH <- new("NullMatrix"))
    ifelse(!isNullObject(object@VPH), VPH <- makeLabels(object@VPH@free, "VPH", package), VPH <- new("NullVector"))
    ifelse(!isNullObject(object@GA), GA <- makeLabels(object@GA@free, "GA", package), GA <- new("NullMatrix"))
    ifelse(!isNullObject(object@TX), TX <- makeLabels(object@TX@free, "TX", package), TX <- new("NullVector"))
    ifelse(!isNullObject(object@MX), MX <- makeLabels(object@MX@free, "MX", package), MX <- new("NullVector"))
    ifelse(!isNullObject(object@KA), KA <- makeLabels(object@KA@free, "KA", package), KA <- new("NullVector"))
    ifelse(!isNullObject(object@TH), TH <- makeLabels(object@TH@free, "TH", package), TH <- new("NullMatrix"))
    ifelse(!isNullObject(object@RTH), RTH <- makeLabels(object@RTH@free, "RTH", package), RTH <- new("NullMatrix"))
    return(new("SimGenLabels", LY = LY, TE = TE, RTE = RTE, VTE = VTE, VY = VY, BE = BE, PS = PS, RPS = RPS, VPS = VPS, VE = VE, AL = AL, 
        ME = ME, TY = TY, MY = MY, LX = LX, TD = TD, RTD = RTD, VTD = VTD, VX = VX, TX = TX, MX = MX, GA = GA, PH = PH, RPH = RPH, VPH = VPH, 
        KA = KA, TH = TH, RTH = RTH, modelType = object@modelType))
})
 
