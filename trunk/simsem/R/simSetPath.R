# simSetPath: This function will create set of matrix that belongs to path
# analysis model. The requirement is to specify indicator correlation and
# regression coefficient matrix.

simSetPath <- function(..., exo = FALSE) {
    W <- getKeywords()
    List <- list(...)
    Names <- names(List)
    keywords <- NULL
    if (exo == FALSE) {
        keywords <- list(W$BE, W$RPS, W$VPS, W$VE, W$AL, W$ME, W$PS)  # Length = 7
    } else {
        keywords <- list(W$BE, W$RPS, W$VPS, W$VE, W$AL, W$ME, W$PS, W$GA, W$RPH, 
            W$VPH, W$KA, W$PH)  # Length = 12
    }
    position <- matchKeywords(Names, keywords)
    if (length(position) != length(unique(position))) 
        stop("Some objects were identified more than once.")
    ifelse(1 %in% position, BE <- List[position == 1], stop("No path coefficient object between factor.ETA"))
    ne <- ncol(run(BE[[1]]))
    if (7 %in% position) {
        PS <- List[position == 7]
        ifelse(2 %in% position, stop("Covariance and correlation cannot be specified at the same time!"), 
            RPS <- list(new("NullSymMatrix")))
        ifelse(3 %in% position, stop("Covariance and variance cannot be specified at the same time!"), 
            VPS <- list(new("NullSimVector")))
        ifelse(4 %in% position, stop("Covariance and total indicator variance cannot be specified at the same time!"), 
            VE <- list(new("NullSimVector")))
    } else {
        PS <- list(new("NullSymMatrix"))
        ifelse(2 %in% position, RPS <- List[position == 2], stop("No residual correlation object between factor.ETA"))
        ifelse(3 %in% position, VPS <- List[position == 3], VPS <- list(new("NullSimVector")))
        ifelse(4 %in% position, VE <- List[position == 4], ifelse(isNullObject(VPS[[1]]), 
            {
                VE <- list(freeVector(1, ne))
                comment(VE[[1]]) <- "default"
            }, VE <- list(new("NullSimVector"))))
    }
    ifelse(6 %in% position, ME <- List[position == 6], ME <- list(new("NullSimVector")))
    ifelse(5 %in% position, AL <- List[position == 5], ifelse(isNullObject(ME[[1]]), 
        {
            AL <- list(freeVector(0, ne))
            comment(AL[[1]]) <- "default"
        }, AL <- list(new("NullSimVector"))))
    Output <- NULL
    if (exo) {
        ifelse(8 %in% position, GA <- List[position == 8], stop("No path coefficient object from Factor.KSI to Factor.ETA"))
        nk <- ncol(run(GA[[1]]))
        if (12 %in% position) {
            PH <- List[position == 12]
            ifelse(9 %in% position, stop("Covariance and correlation cannot be specified at the same time!"), 
                RPH <- list(new("NullSymMatrix")))
            ifelse(10 %in% position, stop("Covariance and variance cannot be specified at the same time!"), 
                VPH <- list(new("NullSimVector")))
        } else {
            PH <- list(new("NullSymMatrix"))
            ifelse(9 %in% position, RPH <- List[position == 9], stop("No correlation object between factor.KSI"))
            ifelse(10 %in% position, VPH <- List[position == 10], {
                VPH <- list(freeVector(1, nk))
                comment(VPH[[1]]) <- "default"
            })
        }
        ifelse(11 %in% position, KA <- List[position == 11], {
            KA <- list(freeVector(0, nk))
            comment(KA[[1]]) <- "default"
        })
        Output <- new("SimSet", BE = BE[[1]], PS = PS[[1]], RPS = RPS[[1]], VPS = VPS[[1]], 
            VE = VE[[1]], AL = AL[[1]], ME = ME[[1]], GA = GA[[1]], PH = PH[[1]], 
            RPH = RPH[[1]], VPH = VPH[[1]], KA = KA[[1]], modelType = "Path.exo")
    } else {
        Output <- new("SimSet", BE = BE[[1]], PS = PS[[1]], RPS = RPS[[1]], VPS = VPS[[1]], 
            VE = VE[[1]], AL = AL[[1]], ME = ME[[1]], modelType = "Path")
    }
    return(Output)
} 
