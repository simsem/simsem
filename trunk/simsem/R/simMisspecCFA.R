# simMisspecCFA: Create a set of matrices that belongs to CFA misspecification
# model.

simMisspecCFA <- function(...) {
    W <- getKeywords()
    List <- list(...)
    Names <- names(List)
    keywords <- list(W$loading, W$errorCor, W$facCor, W$errorVar, W$indicatorVar, 
        W$intercept, W$facMean, W$indicatorMean, W$facVar, W$errorCov, W$facCov)
    position <- matchKeywords(Names, keywords)
    if (length(position) != length(unique(position))) 
        stop("Some objects were identified more than once.")
    ifelse(1 %in% position, LY <- List[position == 1], LY <- list(new("NullSimMatrix")))
    
    if (10 %in% position) {
        TE <- List[position == 10]
        ifelse(2 %in% position, stop("Error covariance and error correlation cannot be specified at the same time!"), 
            RTE <- list(new("NullSymMatrix")))
        ifelse(4 %in% position, stop("Error covariance and error variance cannot be specified at the same time!"), 
            VTE <- list(new("NullSimVector")))
        ifelse(5 %in% position, stop("Error covariance and total indicator variance cannot be specified at the same time!"), 
            VY <- list(new("NullSimVector")))
    } else {
        TE <- list(new("NullSymMatrix"))
        ifelse(2 %in% position, RTE <- List[position == 2], RTE <- list(new("NullSymMatrix")))
        ifelse(4 %in% position, VTE <- List[position == 4], VTE <- list(new("NullSimVector")))
        ifelse(5 %in% position, VY <- List[position == 5], VY <- list(new("NullSimVector")))
        if (!isNullObject(VTE[[1]]) & !isNullObject(VY[[1]])) 
            stop("Please assign either VTE or VY, not both")
    }
    if (11 %in% position) {
        PS <- List[position == 11]
        ifelse(3 %in% position, stop("Factor covariance and factor correlation cannot be specified at the same time!"), 
            RPS <- list(new("NullSymMatrix")))
        ifelse(9 %in% position, stop("Factor covariance and factor variance cannot be specified at the same time!"), 
            VE <- list(new("NullSimVector")))
    } else {
        PS <- list(new("NullSymMatrix"))
        ifelse(3 %in% position, RPS <- List[position == 3], RPS <- list(new("NullSymMatrix")))
        ifelse(9 %in% position, VE <- List[position == 9], VE <- list(new("NullSimVector")))
    }
    ifelse(8 %in% position, MY <- List[position == 8], MY <- list(new("NullSimVector")))
    ifelse(6 %in% position, TY <- List[position == 6], TY <- list(new("NullSimVector")))
    if (!isNullObject(MY[[1]]) & !isNullObject(TY[[1]])) 
        stop("Please assign either MY or TY, not both")
    ifelse(7 %in% position, ME <- List[position == 7], ME <- list(new("NullSimVector")))
    Output <- new("SimMisspec", LY = LY[[1]], PS = PS[[1]], RPS = RPS[[1]], TE = TE[[1]], 
        RTE = RTE[[1]], VE = VE[[1]], VPS = VE[[1]], VTE = VTE[[1]], VY = VY[[1]], 
        TY = TY[[1]], MY = MY[[1]], ME = ME[[1]], AL = ME[[1]], modelType = "CFA")
    return(Output)
} 
