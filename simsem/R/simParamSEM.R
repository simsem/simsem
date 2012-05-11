# simParamSEM: Create a set of free parameter matrices that belongs to SEM
# model.

simParamSEM <- function(..., exo = FALSE) {
    W <- getKeywords()
    List <- list(...)
    Names <- names(List)
    keywords <- NULL
    if (exo == FALSE) {
        keywords <- list(W$LY, W$TY, W$BE, W$AL, W$TE, W$PS)  #Length = 6
    } else {
        keywords <- list(W$LY, W$TY, W$BE, W$AL, W$TE, W$PS, W$LX, W$TX, W$GA, W$KA, 
            W$TD, W$PH, W$TH)  #Length = 13
    }
    position <- matchKeywords(Names, keywords)
    if (length(position) != length(unique(position))) 
        stop("Some objects were identified more than once.")
    if (exo) {
        ifelse(9 %in% position, GA <- List[position == 9][[1]], stop("No path coefficient object between factor.ETA and factor.KSI"))
        ne <- nrow(GA)
        nk <- ncol(GA)
        ifelse(3 %in% position, BE <- List[position == 3][[1]], BE <- matrix(0, ne, 
            ne))
        ifelse(4 %in% position, AL <- List[position == 4][[1]], AL <- rep(NA, ne))
        ifelse(6 %in% position, PS <- List[position == 6][[1]], PS <- diag(NA, ne))
        ifelse(10 %in% position, KA <- List[position == 10][[1]], KA <- rep(NA, nk))
        ifelse(12 %in% position, PH <- List[position == 12][[1]], PH <- matrix(NA, 
            nk, nk))
        ifelse(1 %in% position, LY <- List[position == 1][[1]], stop("No loading object of indicator.Y from factor.ETA in SEM"))
        ny <- nrow(LY)
        ifelse(2 %in% position, TY <- List[position == 2][[1]], TY <- rep(NA, ny))
        ifelse(5 %in% position, TE <- List[position == 5][[1]], TE <- diag(NA, ny))
        ifelse(7 %in% position, LX <- List[position == 7][[1]], stop("No loading object of indicator.X from factor.KSI in SEM"))
        nx <- nrow(LX)
        ifelse(8 %in% position, TX <- List[position == 8][[1]], TX <- rep(NA, nx))
        ifelse(11 %in% position, TD <- List[position == 11][[1]], TD <- diag(NA, 
            nx))
        ifelse(13 %in% position, TH <- List[position == 13][[1]], TH <- matrix(0, 
            nx, ny))
        Output <- new("SimParam", BE = BE, PS = PS, AL = AL, LY = LY, TY = TY, TE = TE, 
            GA = GA, PH = PH, KA = KA, LX = LX, TX = TX, TD = TD, TH = TH, modelType = "SEM.exo")
    } else {
        ifelse(3 %in% position, BE <- List[position == 3][[1]], stop("No path coefficient object between factor.ETA"))
        ne <- nrow(BE)
        ifelse(4 %in% position, AL <- List[position == 4][[1]], AL <- rep(0, ne))
        PS <- NULL
        if (6 %in% position) {
            PS <- List[position == 6][[1]]
        } else {
            PS <- diag(1, ne)
            set <- findRecursiveSet(BE)
            PS[set[[1]], set[[1]]] <- NA
            diag(PS) <- 1
        }
        ifelse(1 %in% position, LY <- List[position == 1][[1]], stop("No loading object of indicator.Y from factor.ETA in SEM"))
        ny <- nrow(LY)
        ifelse(2 %in% position, TY <- List[position == 2][[1]], TY <- rep(NA, ny))
        ifelse(5 %in% position, TE <- List[position == 5][[1]], TE <- diag(NA, ny))
        Output <- new("SimParam", BE = BE, PS = PS, AL = AL, LY = LY, TY = TY, TE = TE, 
            modelType = "SEM")
    }
    return(Output)
} 
