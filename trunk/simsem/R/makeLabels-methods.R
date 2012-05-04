# makeLabels: Make parameter names for each element in matrices or vectors for
# analysis in OpenMx (or other possible packages)

setMethod("makeLabels", signature = "vector", definition = function(object, 
    name, package) {
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

setMethod("makeLabels", signature = "matrix", definition = function(object, 
    name, package, symmetric = FALSE) {
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

setMethod("makeLabels", signature = "SimParam", definition = function(object, 
    package) {
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
    return(new("SimLabels", LY = LY, TE = TE, BE = BE, PS = PS, AL = AL, TY = TY, 
        LX = LX, TD = TD, TX = TX, GA = GA, PH = PH, KA = KA, TH = TH, modelType = object@modelType))
})

setMethod("makeLabels", signature = "VirtualDist", definition = function(object, 
    digit = 3) {
    indivAttr <- slotNames(object)
    val <- sapply(indivAttr, slot, object = object)
    val <- round(val, digit)
    lab <- mapply(paste, indivAttr, "=", val)
    lab <- paste(lab, collapse = ", ")
    return(lab)
}) 
