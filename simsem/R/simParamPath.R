# simSetPath: This function will create set of matrix that belongs to path
# analysis model. The requirement is to specify indicator correlation and
# regression coefficient matrix.

simParamPath <- function(..., exo = FALSE) {
    W <- getKeywords()
    List <- list(...)
    Names <- names(List)
    keywords <- NULL
    if (exo == FALSE) {
        keywords <- list(W$BE, W$AL, W$PS)  # Length = 3
    } else {
        keywords <- list(W$BE, W$AL, W$PS, W$GA, W$KA, W$PH)  # Length = 6
    }
    position <- matchKeywords(Names, keywords)
    if (length(position) != length(unique(position))) 
        stop("Some objects were identified more than once.")
    if (exo) {
        ifelse(4 %in% position, GA <- List[position == 4][[1]], stop("No path coefficient object between factor.ETA and factor.KSI"))
        ne <- nrow(GA)
        nk <- ncol(GA)
        ifelse(1 %in% position, BE <- List[position == 1][[1]], BE <- matrix(0, ne, 
            ne))
        ifelse(2 %in% position, AL <- List[position == 2][[1]], AL <- rep(NA, ne))
        ifelse(3 %in% position, PS <- List[position == 3][[1]], PS <- diag(NA, ne))
        ifelse(5 %in% position, KA <- List[position == 5][[1]], KA <- rep(NA, nk))
        ifelse(6 %in% position, PH <- List[position == 6][[1]], PH <- matrix(NA, 
            nk, nk))
        Output <- new("SimParam", BE = BE, PS = PS, AL = AL, GA = GA, PH = PH, KA = KA, 
            modelType = "Path.exo")
    } else {
        ifelse(1 %in% position, BE <- List[position == 1][[1]], stop("No path coefficient object between factor.ETA"))
        ne <- nrow(BE)
        ifelse(2 %in% position, AL <- List[position == 2][[1]], AL <- rep(NA, ne))
        PS <- NULL
        if (3 %in% position) {
            PS <- List[position == 3][[1]]
        } else {
            PS <- diag(NA, ne)
            set <- findRecursiveSet(BE)
            PS[set[[1]], set[[1]]] <- NA
        }
        Output <- new("SimParam", BE = BE, PS = PS, AL = AL, modelType = "Path")
    }
    return(Output)
} 
