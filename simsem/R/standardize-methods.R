# standardize: Standardize the coefficients of an object

setMethod("standardize", signature = "SimModelOut", definition = function(object) {
    est <- object@coef
    result <- standardize(est)
    return(result)
})

setMethod("standardize", signature = "SimRSet", definition = function(object) {
    type <- object@modelType
    sdIndicator <- sqrt(diag(diag(createImpliedMACS(object)$CM)))
    sdLatent <- NULL
    if (object@modelType == "CFA") {
        sdLatent <- sqrt(diag(diag(object@PS)))
    } else {
        M <- object
        if (M@modelType == "SEM") 
            M@modelType <- "Path"
        if (M@modelType == "SEM.exo") 
            M@modelType <- "Path.exo"
        sdLatent <- sqrt(diag(diag(createImpliedMACS(M)$CM)))
    }
    result <- object
    for (i in 2:length(slotNames(object))) {
        if (!isNullObject(slot(result, slotNames(object)[i]))) {
            slot(result, slotNames(object)[i])[] <- NA
        }
    }
    invSdLatent <- NULL
    invSdIndicator <- NULL
    try(invSdLatent <- solve(sdLatent), silent = TRUE)
    try(invSdIndicator <- solve(sdIndicator), silent = TRUE)
    if (is.null(invSdLatent) || is.null(invSdIndicator)) 
        return(result)
    if (type == "CFA") {
        result@TY <- invSdIndicator %*% object@TY
        result@LY <- invSdIndicator %*% object@LY %*% sdLatent
        result@TE <- invSdIndicator %*% object@TE %*% invSdIndicator
        result@AL <- invSdLatent %*% object@AL
        result@PS <- invSdLatent %*% object@PS %*% invSdLatent
    } else if (type == "Path") {
        result@AL <- invSdIndicator %*% object@AL
        result@PS <- invSdIndicator %*% object@PS %*% invSdIndicator
        result@BE <- invSdIndicator %*% object@BE %*% sdIndicator
    } else if (type == "Path.exo") {
        ny <- nrow(object@GA)
        nx <- ncol(object@GA)
        sdX <- sdIndicator[1:nx, 1:nx]
        sdY <- sdIndicator[(nx + 1):(nx + ny), (nx + 1):(nx + ny)]
        invSdX <- invSdIndicator[1:nx, 1:nx]
        invSdY <- invSdIndicator[(nx + 1):(nx + ny), (nx + 1):(nx + ny)]
        result@AL <- invSdY %*% object@AL
        result@PS <- invSdY %*% object@PS %*% invSdY
        result@BE <- invSdY %*% object@BE %*% sdY
        result@GA <- invSdY %*% object@GA %*% sdX
        result@PH <- invSdX %*% object@PH %*% invSdX
        result@KA <- invSdX %*% object@KA
    } else if (type == "SEM") {
        result@AL <- invSdLatent %*% object@AL
        result@PS <- invSdLatent %*% object@PS %*% invSdLatent
        result@BE <- invSdLatent %*% object@BE %*% sdLatent
        result@TY <- invSdIndicator %*% object@TY
        result@LY <- invSdIndicator %*% object@LY %*% sdLatent
        result@TE <- invSdIndicator %*% object@TE %*% invSdIndicator
    } else if (type == "SEM.exo") {
        nk <- ncol(object@GA)
        ne <- nrow(object@GA)
        nx <- nrow(object@LX)
        ny <- nrow(object@LY)
        sdX <- sdIndicator[1:nx, 1:nx]
        sdY <- sdIndicator[(nx + 1):(nx + ny), (nx + 1):(nx + ny)]
        sdK <- sdLatent[1:nk, 1:nk]
        sdE <- sdLatent[(nk + 1):(nk + ne), (nk + 1):(nk + ne)]
        invSdX <- invSdIndicator[1:nx, 1:nx]
        invSdY <- invSdIndicator[(nx + 1):(nx + ny), (nx + 1):(nx + ny)]
        invSdK <- invSdLatent[1:nk, 1:nk]
        invSdE <- invSdLatent[(nk + 1):(nk + ne), (nk + 1):(nk + ne)]
        result@AL <- invSdE %*% object@AL
        result@PS <- invSdE %*% object@PS %*% invSdE
        result@BE <- invSdE %*% object@BE %*% sdE
        result@TY <- invSdY %*% object@TY
        result@LY <- invSdY %*% object@LY %*% sdE
        result@TE <- invSdY %*% object@TE %*% invSdY
        result@GA <- invSdE %*% object@GA %*% sdK
        result@PH <- invSdK %*% object@PH %*% invSdK
        result@KA <- invSdK %*% object@KA
        result@TX <- invSdX %*% object@TX
        result@LX <- invSdX %*% object@LX %*% sdK
        result@TD <- invSdX %*% object@TD %*% invSdX
        result@TH <- invSdX %*% object@TH %*% invSdY
    } else {
        stop("The coefficient matrix does not have correct model type")
    }
    return(result)
}) 
