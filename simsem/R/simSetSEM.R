# simSetSEM: This function will create set of matrix that belongs to full SEM
# model. The requirement is to specify factor residual correlation matrix,
# regression coefficient matrix, factor loading matrix, and measurement error
# correlation.

simSetSEM <- function(..., exo = FALSE) {
    W <- getKeywords()
    List <- list(...)
    Names <- names(List)
    keywords <- NULL
    if (exo == FALSE) {
        keywords <- list(W$LY, W$RTE, W$VTE, W$VY, W$TY, W$MY, W$BE, W$RPS, W$VPS, 
            W$VE, W$AL, W$ME, W$TE, W$PS)  #Length = 14
    } else {
        keywords <- list(W$LY, W$RTE, W$VTE, W$VY, W$TY, W$MY, W$BE, W$RPS, W$VPS, 
            W$VE, W$AL, W$ME, W$TE, W$PS, W$LX, W$RTD, W$VTD, W$VX, W$TX, W$MX, W$GA, 
            W$RPH, W$VPH, W$KA, W$RTH, W$TD, W$PH, W$TH)  #Length = 28
    }
    position <- matchKeywords(Names, keywords)
    if (length(position) != length(unique(position))) 
        stop("Some objects were identified more than once.")
    ifelse(1 %in% position, LY <- List[position == 1], stop("No loading object of indicator.Y from factor.ETA in SEM"))
    ny <- nrow(run(LY[[1]]))
    ne <- ncol(run(LY[[1]]))
    if (13 %in% position) {
        TE <- List[position == 13]
        ifelse(2 %in% position, stop("Error covariance and error correlation cannot be specified at the same time!"), 
            RTE <- list(new("NullSymMatrix")))
        ifelse(3 %in% position, stop("Error covariance and error variance cannot be specified at the same time!"), 
            VTE <- list(new("NullSimVector")))
        ifelse(4 %in% position, stop("Error covariance and total indicator variance cannot be specified at the same time!"), 
            VY <- list(new("NullSimVector")))
    } else {
        TE <- list(new("NullSymMatrix"))
        ifelse(2 %in% position, RTE <- List[position == 2], stop("No measurement error correlation object between indicator.Y"))
        ifelse(3 %in% position, VTE <- List[position == 3], VTE <- list(new("NullSimVector")))
        ifelse(4 %in% position, VY <- List[position == 4], ifelse(isNullObject(VTE[[1]]), 
            {
                VY <- list(freeVector(1, ny))
                comment(VY[[1]]) <- "default"
            }, VY <- list(new("NullSimVector"))))
    }
    ifelse(6 %in% position, MY <- List[position == 6], MY <- list(new("NullSimVector")))
    ifelse(5 %in% position, TY <- List[position == 5], ifelse(isNullObject(MY[[1]]), 
        {
            TY <- list(freeVector(0, ny))
            comment(TY[[1]]) <- "default"
        }, TY <- list(new("NullSimVector"))))
    ifelse(7 %in% position, BE <- List[position == 7], stop("No path coefficient object between factor.ETA"))
    if (14 %in% position) {
        PS <- List[position == 14]
        ifelse(8 %in% position, stop("Covariance and correlation cannot be specified at the same time!"), 
            RPS <- list(new("NullSymMatrix")))
        ifelse(9 %in% position, stop("Covariance and variance cannot be specified at the same time!"), 
            VPS <- list(new("NullSimVector")))
        ifelse(10 %in% position, stop("Covariance and total indicator variance cannot be specified at the same time!"), 
            VE <- list(new("NullSimVector")))
    } else {
        PS <- list(new("NullSymMatrix"))
        ifelse(8 %in% position, RPS <- List[position == 8], stop("No residual correlation object between factor.ETA"))
        ifelse(9 %in% position, VPS <- List[position == 9], VPS <- list(new("NullSimVector")))
        ifelse(10 %in% position, VE <- List[position == 10], ifelse(isNullObject(VPS[[1]]), 
            {
                VE <- list(constantVector(1, ne))
                comment(VE[[1]]) <- "default"
            }, VE <- list(new("NullSimVector"))))
    }
    ifelse(12 %in% position, ME <- List[position == 12], ME <- list(new("NullSimVector")))
    ifelse(11 %in% position, AL <- List[position == 11], ifelse(isNullObject(ME[[1]]), 
        {
            AL <- list(constantVector(0, ne))
            comment(AL[[1]]) <- "default"
        }, AL <- list(new("NullSimVector"))))
    Output <- NULL
    if (exo) {
        ifelse(15 %in% position, LX <- List[position == 15], stop("No loading object of indicator.X from factor.KSI in SEM"))
        nx <- nrow(run(LX[[1]]))
        nk <- ncol(run(LX[[1]]))
        if (26 %in% position) {
            TD <- List[position == 26]
            ifelse(16 %in% position, stop("Error covariance and error correlation cannot be specified at the same time!"), 
                RTD <- list(new("NullSymMatrix")))
            ifelse(17 %in% position, stop("Error covariance and error variance cannot be specified at the same time!"), 
                VTD <- list(new("NullSimVector")))
            ifelse(18 %in% position, stop("Error covariance and total indicator variance cannot be specified at the same time!"), 
                VX <- list(new("NullSimVector")))
        } else {
            TD <- list(new("NullSymMatrix"))
            ifelse(16 %in% position, RTD <- List[position == 16], stop("No measurement error correlation object between indicator.Y"))
            ifelse(17 %in% position, VTD <- List[position == 17], VTD <- list(new("NullSimVector")))
            ifelse(18 %in% position, VX <- List[position == 18], ifelse(isNullObject(VTD[[1]]), 
                {
                  VX <- list(freeVector(1, nx))
                  comment(VX[[1]]) <- "default"
                }, VX <- list(new("NullSimVector"))))
        }
        ifelse(20 %in% position, MX <- List[position == 20], MX <- list(new("NullSimVector")))
        ifelse(19 %in% position, TX <- List[position == 19], ifelse(isNullObject(MX[[1]]), 
            {
                TX <- list(freeVector(0, nx))
                comment(TX[[1]]) <- "default"
            }, TX <- list(new("NullSimVector"))))
        
        ifelse(21 %in% position, GA <- List[position == 21], stop("No path coefficient object from Factor.KSI to Factor.ETA"))
        if (27 %in% position) {
            PH <- List[position == 27]
            ifelse(22 %in% position, stop("Covariance and correlation cannot be specified at the same time!"), 
                RPH <- list(new("NullSymMatrix")))
            ifelse(23 %in% position, stop("Covariance and variance cannot be specified at the same time!"), 
                VPH <- list(new("NullSimVector")))
        } else {
            PH <- list(new("NullSymMatrix"))
            ifelse(22 %in% position, RPH <- List[position == 22], stop("No correlation object between factor.KSI"))
            ifelse(23 %in% position, VPH <- List[position == 23], {
                VPH <- list(constantVector(1, nk))
                comment(VPH[[1]]) <- "default"
            })
        }
        ifelse(24 %in% position, KA <- List[position == 24], {
            KA <- list(constantVector(0, nk))
            comment(KA[[1]]) <- "default"
        })
        if (28 %in% position) {
            ifelse(25 %in% position, stop("TH and RTH cannot be specified at the same time!"), 
                RTH <- list(new("NullSimMatrix")))
            TH <- List[position == 28]
            temp <- run(TH[[1]])
            if (!((nrow(temp) == nx) & (ncol(temp) == ny))) 
                stop("The number of rows is not equal the number of exogenous indicators or the number of columns is not equal the number of endogenous indicators.")
        } else {
            TH <- list(new("NullSimMatrix"))
            if (25 %in% position) {
                RTH <- List[position == 25]
                temp <- run(RTH[[1]])
                if (!((nrow(temp) == nx) & (ncol(temp) == ny))) 
                  stop("The number of rows is not equal the number of exogenous indicators or the number of columns is not equal the number of endogenous indicators.")
            } else {
                RTH.Data <- matrix(0, nx, ny)
                RTH.Labels <- matrix(NA, nx, ny)
                RTH <- list(new("SimMatrix", free = RTH.Data, param = RTH.Labels))
                comment(RTH[[1]]) <- "default"
            }
        }
        Output <- new("SimSet", LY = LY[[1]], TE = TE[[1]], RTE = RTE[[1]], VTE = VTE[[1]], 
            VY = VY[[1]], MY = MY[[1]], TY = TY[[1]], BE = BE[[1]], PS = PS[[1]], 
            RPS = RPS[[1]], VPS = VPS[[1]], VE = VE[[1]], AL = AL[[1]], ME = ME[[1]], 
            LX = LX[[1]], TD = TD[[1]], RTD = RTD[[1]], VTD = VTD[[1]], VX = VX[[1]], 
            MX = MX[[1]], TX = TX[[1]], GA = GA[[1]], PH = PH[[1]], RPH = RPH[[1]], 
            VPH = VPH[[1]], KA = KA[[1]], TH = TH[[1]], RTH = RTH[[1]], modelType = "SEM.exo")
    } else {
        Output <- new("SimSet", LY = LY[[1]], TE = TE[[1]], RTE = RTE[[1]], VTE = VTE[[1]], 
            VY = VY[[1]], MY = MY[[1]], TY = TY[[1]], BE = BE[[1]], PS = PS[[1]], 
            RPS = RPS[[1]], VPS = VPS[[1]], VE = VE[[1]], AL = AL[[1]], ME = ME[[1]], 
            modelType = "SEM")
    }
    return(Output)
} 
