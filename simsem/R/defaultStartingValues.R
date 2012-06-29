# defaultStartingValues: Make ad hoc starting values. Use for OpenMx package

defaultStartingValues <- function(object) {
    ifelse(isNullObject(object@LY), LY <- new("NullMatrix"), {
        LY <- matrix(NA, nrow(object@LY), ncol(object@LY))
        LY[is.na(object@LY)] <- 0.7
    })
    ifelse(isNullObject(object@TE), TE <- new("NullMatrix"), {
        TE <- matrix(NA, nrow(object@TE), ncol(object@TE))
        TE[is.na(object@TE)] <- 0.49
        TE[is.na(object@TE) & (upper.tri(TE) | lower.tri(TE))] <- 0.2
    })
    ifelse(isNullObject(object@PS), PS <- new("NullMatrix"), {
        PS <- matrix(NA, nrow(object@PS), ncol(object@PS))
        PS[is.na(object@PS)] <- 1
        PS[is.na(object@PS) & (upper.tri(PS) | lower.tri(PS))] <- 0.2
    })
    ifelse(isNullObject(object@BE), BE <- new("NullMatrix"), {
        BE <- matrix(NA, nrow(object@BE), ncol(object@BE))
        BE[is.na(object@BE)] <- 0.3
    })
    ifelse(isNullObject(object@TY), TY <- new("NullVector"), {
        TY <- rep(NA, length(object@TY))
        TY[is.na(object@TY)] <- 0
    })
    ifelse(isNullObject(object@AL), AL <- new("NullVector"), {
        AL <- rep(NA, length(object@AL))
        AL[is.na(object@AL)] <- 0
    })
    ifelse(isNullObject(object@LX), LX <- new("NullMatrix"), {
        LX <- matrix(NA, nrow(object@LX), ncol(object@LX))
        LX[is.na(object@LX)] <- 0.7
    })
    ifelse(isNullObject(object@TD), TD <- new("NullMatrix"), {
        TD <- matrix(NA, nrow(object@TD), ncol(object@TD))
        TD[is.na(object@TD)] <- 0.49
        TD[is.na(object@TD) & (upper.tri(TD) | lower.tri(TD))] <- 0.2
    })
    ifelse(isNullObject(object@PH), PH <- new("NullMatrix"), {
        PH <- matrix(NA, nrow(object@PH), ncol(object@PH))
        PH[is.na(object@PH)] <- 1
        PH[is.na(object@PH) & (upper.tri(PH) | lower.tri(PH))] <- 0.2
    })
    ifelse(isNullObject(object@GA), GA <- new("NullMatrix"), {
        GA <- matrix(NA, nrow(object@GA), ncol(object@GA))
        GA[is.na(object@GA)] <- 0.3
    })
    ifelse(isNullObject(object@TX), TX <- new("NullVector"), {
        TX <- rep(NA, length(object@TX))
        TX[is.na(object@TX)] <- 0
    })
    ifelse(isNullObject(object@KA), KA <- new("NullVector"), {
        KA <- rep(NA, length(object@KA))
        KA[is.na(object@KA)] <- 0
    })
    ifelse(isNullObject(object@TH), TH <- new("NullMatrix"), {
        TH <- matrix(NA, nrow(object@TH), ncol(object@TH))
        TH[is.na(object@TH)] <- 0.7
    })
    return(new("SimRSet", LY = LY, TE = TE, BE = BE, PS = PS, AL = AL, TY = TY, LX = LX, TD = TD, TX = TX, GA = GA, PH = PH, KA = KA, 
        TH = TH, modelType = object@modelType))
} 
