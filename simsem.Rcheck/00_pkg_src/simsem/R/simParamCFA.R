# simParamCFA: Create a set of free parameter matrices that belongs to CFA model.

simParamCFA <- function(...) {
    W <- getKeywords()
    List <- list(...)
    Names <- names(List)
    keywords <- list(W$loading, W$intercept, W$facMean, W$errorCov, W$facCov)  # 5 total
    position <- matchKeywords(Names, keywords)
    if (length(position) != length(unique(position))) 
        stop("Some objects were identified more than once.")
    ifelse(1 %in% position, LY <- List[position == 1][[1]], stop("No loading object in CFA"))
    ni <- nrow(LY)
    nk <- ncol(LY)
    ifelse(4 %in% position, TE <- List[position == 4][[1]], TE <- diag(NA, ni))
    ifelse(5 %in% position, PS <- List[position == 5][[1]], {
        PS <- matrix(NA, nk, nk)
        diag(PS) <- 1
    })
    ifelse(2 %in% position, TY <- List[position == 2][[1]], TY <- rep(NA, ni))
    ifelse(3 %in% position, AL <- List[position == 3][[1]], AL <- rep(1, nk))
    Output <- new("SimParam", LY = LY, PS = PS, TE = TE, TY = TY, AL = AL, modelType = "CFA")
    return(Output)
} 
