# subtractObject: Make a subtract of each element

setMethod("subtractObject", signature(object1 = "SimRSet", object2 = "SimRSet"), definition = function(object1, object2) {
    modelType <- object1@modelType
    exo <- (modelType == "SEM.exo") | (modelType == "Path.exo")
    nx <- ny <- ne <- nk <- 0
    if (modelType == "CFA") {
        object1@LY <- object1@LY - object2@LY
        object1@PS <- object1@PS - object2@PS
        object1@TE <- object1@TE - object2@TE
        object1@TY <- object1@TY - object2@TY
        object1@AL <- object1@AL - object2@AL
    } else if (modelType == "Path") {
        object1@PS <- object1@PS - object2@PS
        object1@AL <- object1@AL - object2@AL
        object1@BE <- object1@BE - object2@BE
    } else if (modelType == "Path.exo") {
        object1@PS <- object1@PS - object2@PS
        object1@AL <- object1@AL - object2@AL
        object1@BE <- object1@BE - object2@BE
        object1@PH <- object1@PH - object2@PH
        object1@KA <- object1@KA - object2@KA
        object1@GA <- object1@GA - object2@GA
    } else if (modelType == "SEM") {
        object1@LY <- object1@LY - object2@LY
        object1@PS <- object1@PS - object2@PS
        object1@TE <- object1@TE - object2@TE
        object1@TY <- object1@TY - object2@TY
        object1@AL <- object1@AL - object2@AL
        object1@BE <- object1@BE - object2@BE
    } else if (modelType == "SEM.exo") {
        object1@LY <- object1@LY - object2@LY
        object1@PS <- object1@PS - object2@PS
        object1@TE <- object1@TE - object2@TE
        object1@TY <- object1@TY - object2@TY
        object1@AL <- object1@AL - object2@AL
        object1@BE <- object1@BE - object2@BE
        object1@LX <- object1@LX - object2@LX
        object1@PH <- object1@PH - object2@PH
        object1@TD <- object1@TD - object2@TD
        object1@TX <- object1@TX - object2@TX
        object1@KA <- object1@KA - object2@KA
        object1@GA <- object1@GA - object2@GA
        object1@TH <- object1@TH - object2@TH
    } else {
        stop("something wrong!")
    }
    return(object1)
}) 
