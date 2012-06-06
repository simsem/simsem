# simMisspecSEM: Create a set of matrices that belongs to SEM misspecification model.

simMisspecSEM <- function(..., exo = FALSE, conBeforeMis = TRUE, misBeforeFill = TRUE, misfitType = "rmsea", misfitBound = new("NullVector"), 
    averageNumMisspec = FALSE, optMisfit = "none", numIter = 20) {
    if (!isNullObject(misfitBound)) {
        if (length(misfitBound) == 2) {
            if (misfitBound[1] >= misfitBound[2]) 
                stop("The lower bound is higher than the upper bound")
        } else {
            stop("misfitBound must include only two numbers for lower and upper bound")
        }
    }
    W <- getKeywords()
    optMin <- W$optMin
    optMax <- W$optMax
    optNone <- W$optNone
    optMisfit <- tolower(optMisfit)
    if (optMisfit != "none") {
        if (optMisfit %in% optNone) 
            optMisfit <- optNone[1]
        if (optMisfit %in% optMax) 
            optMisfit <- optMax[1]
        if (optMisfit %in% optMin) 
            optMisfit <- optMin[1]
    }
    if ((optMisfit != "none") && !isNullObject(misfitBound)) {
        stop("The optimized misfit approach does not work with the misfit bound approach.")
    }
    
    List <- list(...)
    Names <- names(List)
    keywords <- NULL
    if (exo == FALSE) {
        keywords <- list(W$LY, W$RTE, W$VTE, W$VY, W$TY, W$MY, W$BE, W$RPS, W$VPS, W$VE, W$AL, W$ME, W$TE, W$PS)  #Length = 14
    } else {
        keywords <- list(W$LY, W$RTE, W$VTE, W$VY, W$TY, W$MY, W$BE, W$RPS, W$VPS, W$VE, W$AL, W$ME, W$TE, W$PS, W$LX, W$RTD, W$VTD, W$VX, 
            W$TX, W$MX, W$GA, W$RPH, W$VPH, W$KA, W$RTH, W$TD, W$PH, W$TH)  #Length = 28
    }
    position <- matchKeywords(Names, keywords)
    if (length(position) != length(unique(position))) 
        stop("Some objects were identified more than once.")
    ifelse(1 %in% position, LY <- List[position == 1], LY <- list(new("NullSimMatrix")))
    if (13 %in% position) {
        TE <- List[position == 13]
        ifelse(2 %in% position, stop("Error covariance and error correlation cannot be specified at the same time!"), RTE <- list(new("NullSymMatrix")))
        ifelse(3 %in% position, stop("Error covariance and error variance cannot be specified at the same time!"), VTE <- list(new("NullSimVector")))
        ifelse(4 %in% position, stop("Error covariance and total indicator variance cannot be specified at the same time!"), VY <- list(new("NullSimVector")))
    } else {
        TE <- list(new("NullSymMatrix"))
        ifelse(2 %in% position, RTE <- List[position == 2], RTE <- list(new("NullSymMatrix")))
        ifelse(3 %in% position, VTE <- List[position == 3], VTE <- list(new("NullSimVector")))
        ifelse(4 %in% position, VY <- List[position == 4], VY <- list(new("NullSimVector")))
        if (!isNullObject(VTE[[1]]) & !isNullObject(VY[[1]])) 
            stop("Please assign either VTE or VY, not both")
    }
    ifelse(6 %in% position, MY <- List[position == 6], MY <- list(new("NullSimVector")))
    ifelse(5 %in% position, TY <- List[position == 5], TY <- list(new("NullSimVector")))
    if (!isNullObject(MY[[1]]) & !isNullObject(TY[[1]])) 
        stop("Please assign either MY or TY, not both")
    ifelse(7 %in% position, BE <- List[position == 7], BE <- list(new("NullSimMatrix")))
    if (14 %in% position) {
        PS <- List[position == 14]
        ifelse(8 %in% position, stop("Covariance and correlation cannot be specified at the same time!"), RPS <- list(new("NullSymMatrix")))
        ifelse(9 %in% position, stop("Covariance and variance cannot be specified at the same time!"), VPS <- list(new("NullSimVector")))
        ifelse(10 %in% position, stop("Covariance and total indicator variance cannot be specified at the same time!"), VE <- list(new("NullSimVector")))
    } else {
        PS <- list(new("NullSymMatrix"))
        ifelse(8 %in% position, RPS <- List[position == 8], RPS <- list(new("NullSymMatrix")))
        ifelse(9 %in% position, VPS <- List[position == 9], VPS <- list(new("NullSimVector")))
        ifelse(10 %in% position, VE <- List[position == 10], VE <- list(new("NullSimVector")))
        if (!isNullObject(VPS[[1]]) & !isNullObject(VE[[1]])) 
            stop("Please assign either VPS or VE, not both")
    }
    ifelse(12 %in% position, ME <- List[position == 12], ME <- list(new("NullSimVector")))
    ifelse(11 %in% position, AL <- List[position == 11], AL <- list(new("NullSimVector")))
    if (!isNullObject(ME[[1]]) & !isNullObject(AL[[1]])) 
        stop("Please assign either ME or AL, not both")
    Output <- NULL
    if (exo) {
        ifelse(15 %in% position, LX <- List[position == 15], LX <- list(new("NullSimMatrix")))
        if (26 %in% position) {
            TD <- List[position == 26]
            ifelse(16 %in% position, stop("Error covariance and error correlation cannot be specified at the same time!"), RTD <- list(new("NullSymMatrix")))
            ifelse(17 %in% position, stop("Error covariance and error variance cannot be specified at the same time!"), VTD <- list(new("NullSimVector")))
            ifelse(18 %in% position, stop("Error covariance and total indicator variance cannot be specified at the same time!"), VX <- list(new("NullSimVector")))
        } else {
            TD <- list(new("NullSymMatrix"))
            ifelse(16 %in% position, RTD <- List[position == 16], RTD <- list(new("NullSymMatrix")))
            ifelse(17 %in% position, VTD <- List[position == 17], VTD <- list(new("NullSimVector")))
            ifelse(18 %in% position, VX <- List[position == 18], VX <- list(new("NullSimVector")))
            if (!isNullObject(VTD[[1]]) & !isNullObject(VX[[1]])) 
                stop("Please assign either VTD or VX, not both")
        }
        ifelse(20 %in% position, MX <- List[position == 20], MX <- list(new("NullSimVector")))
        ifelse(19 %in% position, TX <- List[position == 19], TX <- list(new("NullSimVector")))
        if (!isNullObject(MX[[1]]) & !isNullObject(TX[[1]])) 
            stop("Please assign either MX or TX, not both")
        ifelse(21 %in% position, GA <- List[position == 21], GA <- list(new("NullSimMatrix")))
        if (27 %in% position) {
            PH <- List[position == 27]
            ifelse(22 %in% position, stop("Covariance and correlation cannot be specified at the same time!"), RPH <- list(new("NullSymMatrix")))
            ifelse(23 %in% position, stop("Covariance and variance cannot be specified at the same time!"), VPH <- list(new("NullSimVector")))
        } else {
            PH <- list(new("NullSymMatrix"))
            ifelse(22 %in% position, RPH <- List[position == 22], RPH <- list(new("NullSymMatrix")))
            ifelse(23 %in% position, VPH <- List[position == 23], VPH <- list(new("NullSimVector")))
        }
        ifelse(24 %in% position, KA <- List[position == 24], KA <- list(new("NullSimVector")))
        if (28 %in% position) {
            ifelse(25 %in% position, stop("TH and RTH cannot be specified at the same time!"), RTH <- list(new("NullSimMatrix")))
            TH <- List[position == 28]
            print("If TH is specified, please make sure that the TH is explicitly specified in the simSetSEM function.")
        } else {
            ifelse(25 %in% position, RTH <- List[position == 25], RTH <- list(new("NullSimMatrix")))
            TH <- list(new("NullSimMatrix"))
        }
        Output <- new("SimMisspec", LY = LY[[1]], TE = TE[[1]], RTE = RTE[[1]], VTE = VTE[[1]], VY = VY[[1]], MY = MY[[1]], TY = TY[[1]], 
            BE = BE[[1]], PS = PS[[1]], RPS = RPS[[1]], VPS = VPS[[1]], VE = VE[[1]], AL = AL[[1]], ME = ME[[1]], LX = LX[[1]], TD = TD[[1]], 
            RTD = RTD[[1]], VTD = VTD[[1]], VX = VX[[1]], MX = MX[[1]], TX = TX[[1]], GA = GA[[1]], PH = PH[[1]], RPH = RPH[[1]], VPH = VPH[[1]], 
            KA = KA[[1]], TH = TH[[1]], RTH = RTH[[1]], modelType = "SEM.exo", conBeforeMis = conBeforeMis, misBeforeFill = misBeforeFill, 
            misfitType = misfitType, misfitBound = misfitBound, averageNumMisspec = averageNumMisspec)
    } else {
        Output <- new("SimMisspec", LY = LY[[1]], TE = TE[[1]], RTE = RTE[[1]], VTE = VTE[[1]], VY = VY[[1]], MY = MY[[1]], TY = TY[[1]], 
            BE = BE[[1]], PS = PS[[1]], RPS = RPS[[1]], VPS = VPS[[1]], VE = VE[[1]], AL = AL[[1]], ME = ME[[1]], modelType = "SEM", conBeforeMis = conBeforeMis, 
            misBeforeFill = misBeforeFill, misfitType = misfitType, misfitBound = misfitBound, averageNumMisspec = averageNumMisspec, 
            optMisfit = optMisfit, numIter = numIter)
    }
    return(Output)
} 
