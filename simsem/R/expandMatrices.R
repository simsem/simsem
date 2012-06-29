# expandMatrices: Expand the set of covariance matrices into the set of covariance/correlation/variance objects

expandMatrices <- function(object) {
    if (!is(object, "SimRSet")) 
        stop("The object is not a SimRSet object")
    modelType <- object@modelType
    BE <- object@BE
    AL <- object@AL
    RTE <- new("NullMatrix")
    LY <- object@LY
    TY <- object@TY
    RPH <- new("NullMatrix")
    GA <- object@GA
    KA <- object@KA
    RTD <- new("NullMatrix")
    LX <- object@LX
    TX <- object@TX
    TH <- object@TH
    RTH <- new("NullMatrix")
    RPS <- cov2cor(object@PS)
    VPS <- diag(object@PS)
    VPH <- new("NullVector")
    VTD <- new("NullVector")
    VTE <- new("NullVector")
    VX <- new("NullVector")
    VY <- new("NullVector")
    VE <- new("NullVector")
    MX <- new("NullVector")
    MY <- new("NullVector")
    ME <- new("NullVector")
    if (modelType == "CFA" | modelType == "SEM" | modelType == "SEM.exo") {
        VTE <- diag(object@TE)
        RTE <- cov2cor(object@TE)
    }
    if (modelType == "Path.exo" | modelType == "SEM.exo") {
        VPH <- diag(object@PH)
        RPH <- cov2cor(object@PH)
    }
    if (modelType == "SEM.exo") {
        VTD <- diag(object@TD)
        RTD <- cov2cor(object@TD)
    }
    if (modelType == "CFA") {
        ME <- AL
        VE <- VPS
        VY <- findIndTotalVar(LY, object@PS, VTE)
        MY <- findIndMean(LY, ME, TY)
    } else if (modelType == "Path") {
        VE <- findFactorTotalVar(BE, RPS, VPS)
        ME <- findFactorMean(BE, AL)
    } else if (modelType == "Path.exo") {
        nx <- ncol(GA)
        ny <- nrow(GA)
        temp.BE <- combinePathExoEndo(GA, BE)
        temp.RPS <- combineLatentCorExoEndo(RPH, RPS)
        temp.VE <- findFactorTotalVar(temp.BE, temp.RPS, c(VPH, VPS))
        VE <- temp.VE[(nx + 1):(nx + ny)]
        temp.ME <- findFactorMean(temp.BE, c(KA, AL))
        ME <- temp.ME[(nx + 1):(nx + ny)]
    } else if (modelType == "SEM") {
        VE <- findFactorTotalVar(BE, RPS, VPS)
        ME <- findFactorMean(BE, AL)
        facCov <- findFactorTotalCov(BE, object@PS)
        VY <- findIndTotalVar(LY, facCov, VTE)
        MY <- findIndMean(LY, ME, TY)
    } else if (modelType == "SEM.exo") {
        nk <- ncol(GA)
        ne <- nrow(GA)
        temp.BE <- combinePathExoEndo(GA, BE)
        temp.RPS <- combineLatentCorExoEndo(RPH, RPS)
        temp.VE <- findFactorTotalVar(temp.BE, temp.RPS, c(VPH, VPS))
        VE <- temp.VE[(nk + 1):(nk + ne)]
        temp.ME <- findFactorMean(temp.BE, c(KA, AL))
        ME <- temp.ME[(nk + 1):(nk + ne)]
        facCov <- findFactorTotalCov(combinePathExoEndo(GA, BE), combineLatentCorExoEndo(object@PH, object@PS))
        VY <- findIndTotalVar(LY, facCov[(nk + 1):(nk + ne), (nk + 1):(nk + ne)], VTE)
        MY <- findIndMean(LY, ME, TY)
        VX <- findIndTotalVar(LX, facCov[1:nk, 1:nk], VTD)
        MX <- findIndMean(LX, KA, TX)
        RTH <- solve(sqrt(diag(VTD))) %*% TH %*% solve(sqrt(diag(VTE)))
    }
    return(new("MatrixSet", modelType = modelType, LY = LY, VTE = VTE, TE = object@TE, RTE = RTE, VY = VY, TY = TY, MY = MY, BE = BE, 
        VPS = VPS, PS = object@PS, RPS = RPS, VE = VE, AL = AL, ME = ME, LX = LX, VTD = VTD, TD = object@TD, RTD = RTD, VX = VX, TX = TX, 
        MX = MX, GA = GA, VPH = VPH, PH = object@PH, RPH = RPH, KA = KA, TH = object@TH, RTH = RTH))
} 
