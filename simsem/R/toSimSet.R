# toSimSet: Create a SimSet object from SimModelOut

setMethod("toSimSet", signature(out = "SimModelOut"), definition = function(out, usedStd = TRUE) {
    start <- out@coef
    if (usedStd) 
        start <- standardize(out)
    result <- toSimSet(start, out@param)
    return(result)
})

setMethod("toSimSet", signature(out = "SimRSet"), definition = function(out, param) {
    start <- out
    LY <- new("NullSimMatrix")
    TY <- new("NullSimVector")
    BE <- new("NullSimMatrix")
    AL <- new("NullSimVector")
    TE <- new("NullSymMatrix")
    PS <- new("NullSymMatrix")
    LX <- new("NullSimMatrix")
    TX <- new("NullSimVector")
    GA <- new("NullSimMatrix")
    KA <- new("NullSimVector")
    TD <- new("NullSymMatrix")
    PH <- new("NullSymMatrix")
    TH <- new("NullSimMatrix")
    if (!isNullObject(param@LY)) 
        LY <- simMatrix(free = param@LY, value = start@LY)
    if (!isNullObject(param@TY)) 
        TY <- simVector(free = param@TY, value = start@TY)
    if (!isNullObject(param@BE)) 
        BE <- simMatrix(free = param@BE, value = start@BE)
    if (!isNullObject(param@AL)) 
        AL <- simVector(free = param@AL, value = start@AL)
    if (!isNullObject(param@TE)) 
        TE <- symMatrix(free = param@TE, value = start@TE)
    if (!isNullObject(param@PS)) 
        PS <- symMatrix(free = param@PS, value = start@PS)
    if (!isNullObject(param@LX)) 
        LX <- simMatrix(free = param@LX, value = start@LX)
    if (!isNullObject(param@TX)) 
        TX <- simVector(free = param@TX, value = start@TX)
    if (!isNullObject(param@GA)) 
        GA <- simMatrix(free = param@GA, value = start@GA)
    if (!isNullObject(param@KA)) 
        KA <- simVector(free = param@KA, value = start@KA)
    if (!isNullObject(param@TD)) 
        TD <- symMatrix(free = param@TD, value = start@TD)
    if (!isNullObject(param@PH)) 
        PH <- symMatrix(free = param@PH, value = start@PH)
    if (!isNullObject(param@TH)) 
        TH <- simMatrix(free = param@TH, value = start@TH)
    modelType <- param@modelType
    result <- NULL
    if (modelType == "CFA") {
        result <- simSetCFA(LY = LY, TY = TY, AL = AL, TE = TE, PS = PS)
    } else if (modelType == "Path") {
        result <- simSetPath(BE = BE, AL = AL, PS = PS)
    } else if (modelType == "Path.exo") {
        result <- simSetPath(BE = BE, AL = AL, PS = PS, GA = GA, KA = KA, PH = PH, exo = TRUE)
    } else if (modelType == "SEM") {
        result <- simSetSEM(LY = LY, TY = TY, BE = BE, AL = AL, TE = TE, PS = PS)
    } else if (modelType == "SEM.exo") {
        result <- simSetSEM(LY = LY, TY = TY, BE = BE, AL = AL, TE = TE, PS = PS, LX = LX, TX = TX, GA = GA, KA = KA, TD = TD, PH = PH, 
            TH = TH, exo = TRUE)
    } else {
        stop("Something is wrong!")
    }
    return(result)
}) 
