# simMisspecPath: Create a set of matrices that belongs to path analysis misspecification model.

simMisspecPath <- function(..., exo = FALSE, conBeforeMis = TRUE, misBeforeFill = TRUE, misfitType = "rmsea", misfitBound = new("NullVector"), averageNumMisspec = FALSE, optMisfit = "none", 
    numIter = 20) {
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
        keywords <- list(W$BE, W$RPS, W$VPS, W$VE, W$AL, W$ME, W$PS)  # Length = 7
    } else {
        keywords <- list(W$BE, W$RPS, W$VPS, W$VE, W$AL, W$ME, W$PS, W$GA, W$RPH, W$VPH, W$KA, W$PH)  # Length = 12
    }
    position <- matchKeywords(Names, keywords)
    if (length(position) != length(unique(position))) 
        stop("Some objects were identified more than once.")
    ifelse(1 %in% position, BE <- List[position == 1], BE <- list(new("NullSimMatrix")))
    if (7 %in% position) {
        PS <- List[position == 7]
        ifelse(2 %in% position, stop("Covariance and correlation cannot be specified at the same time!"), RPS <- list(new("NullSymMatrix")))
        ifelse(3 %in% position, stop("Covariance and variance cannot be specified at the same time!"), VPS <- list(new("NullSimVector")))
        ifelse(4 %in% position, stop("Covariance and total indicator variance cannot be specified at the same time!"), VE <- list(new("NullSimVector")))
    } else {
        PS <- list(new("NullSymMatrix"))
        ifelse(2 %in% position, RPS <- List[position == 2], RPS <- list(new("NullSymMatrix")))
        ifelse(3 %in% position, VPS <- List[position == 3], VPS <- list(new("NullSimVector")))
        ifelse(4 %in% position, VE <- List[position == 4], VE <- list(new("NullSimVector")))
    }
    if (!isNullObject(VPS[[1]]) & !isNullObject(VE[[1]])) 
        stop("Please assign either VPS or VE, not both")
    ifelse(6 %in% position, ME <- List[position == 6], ME <- list(new("NullSimVector")))
    ifelse(5 %in% position, AL <- List[position == 5], AL <- list(new("NullSimVector")))
    if (!isNullObject(ME[[1]]) & !isNullObject(AL[[1]])) 
        stop("Please assign either ME or AL, not both")
    Output <- NULL
    if (exo) {
        ifelse(8 %in% position, GA <- List[position == 8], GA <- list(new("NullSimMatrix")))
        if (12 %in% position) {
            PH <- List[position == 12]
            ifelse(9 %in% position, stop("Covariance and correlation cannot be specified at the same time!"), RPH <- list(new("NullSymMatrix")))
            ifelse(10 %in% position, stop("Covariance and variance cannot be specified at the same time!"), VPH <- list(new("NullSimVector")))
        } else {
            PH <- list(new("NullSymMatrix"))
            ifelse(9 %in% position, RPH <- List[position == 9], RPH <- list(new("NullSymMatrix")))
            ifelse(10 %in% position, VPH <- List[position == 10], VPH <- list(new("NullSimVector")))
        }
        ifelse(11 %in% position, KA <- List[position == 11], KA <- list(new("NullSimVector")))
        Output <- new("SimMisspec", BE = BE[[1]], PS = PS[[1]], RPS = RPS[[1]], VPS = VPS[[1]], VE = VE[[1]], AL = AL[[1]], ME = ME[[1]], GA = GA[[1]], PH = PH[[1]], RPH = RPH[[1]], VPH = VPH[[1]], 
            KA = KA[[1]], modelType = "Path.exo", conBeforeMis = conBeforeMis, misBeforeFill = misBeforeFill, misfitType = misfitType, misfitBound = misfitBound, averageNumMisspec = averageNumMisspec)
    } else {
        Output <- new("SimMisspec", BE = BE[[1]], PS = PS[[1]], RPS = RPS[[1]], VPS = VPS[[1]], VE = VE[[1]], AL = AL[[1]], ME = ME[[1]], modelType = "Path", conBeforeMis = conBeforeMis, misBeforeFill = misBeforeFill, 
            misfitType = misfitType, misfitBound = misfitBound, averageNumMisspec = averageNumMisspec, optMisfit = optMisfit, numIter = numIter)
    }
    return(Output)
} 
