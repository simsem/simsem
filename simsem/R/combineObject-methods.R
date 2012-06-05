# combineObject: Combine by summing or binding two objects together.

setMethod("combineObject", signature(object1 = "SimMatrix", object2 = "SimMatrix"), definition = function(object1, object2) {
    type <- "SimMatrix"
    if (is(object1, "SymMatrix") && is(object2, "SymMatrix")) 
        type <- "SymMatrix"
    Labels1 <- object1@value
    Labels2 <- object2@value
    Nrow <- nrow(Labels1)
    Ncol <- ncol(Labels2)
    new.Labels <- matrix(NA, Nrow, Ncol)
    new.Data <- matrix(NA, Nrow, Ncol)
    if ((Nrow != nrow(Labels2)) | (Ncol != ncol(Labels2))) 
        stop("The dimension of objects are not equal")
    for (i in 1:Nrow) {
        for (j in 1:Ncol) {
            if (is.na(Labels1[i, j])) {
                if (is.na(Labels2[i, j])) {
                  new.Data[i, j] <- object1@free[i, j]
                } else {
                  new.Labels[i, j] <- Labels2[i, j]
                }
            } else {
                if (is.na(Labels2[i, j])) {
                  new.Labels[i, j] <- Labels1[i, j]
                } else {
                  new.Labels[i, j] <- Labels2[i, j]
                }
            }
        }
    }
    return(new(type, Data = new.Data, Labels = new.Labels))
})

setMethod("combineObject", signature(object1 = "SimVector", object2 = "SimVector"), definition = function(object1, object2) {
    Labels1 <- object1@value
    Labels2 <- object2@value
    Length <- length(Labels1)
    new.Labels <- rep(NA, Length)
    new.Data <- rep(NA, Length)
    if (Length != length(Labels2)) 
        stop("The dimension of objects are not equal")
    for (i in 1:Length) {
        if (is.na(Labels1[i])) {
            if (is.na(Labels2[i])) {
                new.Data[i] <- object1@free[i]
            } else {
                new.Labels[i] <- Labels2[i]
            }
        } else {
            if (is.na(Labels2[i])) {
                new.Labels[i] <- Labels1[i]
            } else {
                new.Labels[i] <- Labels2[i]
            }
        }
    }
    return(new(type, Data = new.Data, Labels = new.Labels))
})

setMethod("combineObject", signature(object1 = "vector", object2 = "vector"), definition = function(object1, object2) {
    if (isNullObject(object1)) {
        if (isNullObject(object2)) {
            return(new("NullVector"))
        } else {
            stop("Please make sure that \n\n1) The trivially misspecified matrix set is put as a second argument. \n\n2) Any of trivially misspecified matrices are not null in the main set.")
        }
    } else {
        if (isNullObject(object2)) {
            return(object1)
        } else {
            ifelse(length(object1) == length(object2), return(object1 + object2), stop("Length of vectors are not equal."))
        }
    }
})

setMethod("combineObject", signature(object1 = "matrix", object2 = "matrix"), definition = function(object1, object2, correlation = FALSE) {
    if (isNullObject(object1)) {
        if (isNullObject(object2)) {
            return(new("NullMatrix"))
        } else {
            stop("Please make sure that \n\n1) The trivially misspecified matrix set is put as a second argument. \n\n2) Any of trivially misspecified matrices are not null in the main set.")
        }
    } else {
        if (isNullObject(object2)) {
            return(object1)
        } else {
            if (sum(dim(object1) != dim(object2)) == 0) {
                if (correlation == TRUE) {
                  temp <- object1 + object2
                  diag(temp) <- 1
                  return(temp)
                } else {
                  return(object1 + object2)
                }
            } else {
                stop("Dimension of matrices are not equal.")
            }
        }
    }
})

setMethod("combineObject", signature(object1 = "MatrixSet", object2 = "MatrixSet"), definition = function(object1, object2) {
    LY <- combineObject(object1@LY, object2@LY)
    VTE <- combineObject(object1@VTE, object2@VTE)
    TE <- combineObject(object1@TE, object2@TE)
    RTE <- combineObject(object1@RTE, object2@RTE, correlation = TRUE)
    VY <- combineObject(object1@VY, object2@VY)
    TY <- combineObject(object1@TY, object2@TY)
    MY <- combineObject(object1@MY, object2@MY)
    BE <- combineObject(object1@BE, object2@BE)
    VPS <- combineObject(object1@VPS, object2@VPS)
    PS <- combineObject(object1@PS, object2@PS)
    RPS <- combineObject(object1@RPS, object2@RPS, correlation = TRUE)
    VE <- combineObject(object1@VE, object2@VE)
    AL <- combineObject(object1@AL, object2@AL)
    ME <- combineObject(object1@ME, object2@ME)
    LX <- combineObject(object1@LX, object2@LX)
    VTD <- combineObject(object1@VTD, object2@VTD)
    TD <- combineObject(object1@TD, object2@TD)
    RTD <- combineObject(object1@RTD, object2@RTD, correlation = TRUE)
    VX <- combineObject(object1@VX, object2@VX)
    TX <- combineObject(object1@TX, object2@TX)
    MX <- combineObject(object1@MX, object2@MX)
    GA <- combineObject(object1@GA, object2@GA)
    VPH <- combineObject(object1@VPH, object2@VPH)
    PH <- combineObject(object1@PH, object2@PH)
    RPH <- combineObject(object1@RPH, object2@RPH, correlation = TRUE)
    KA <- combineObject(object1@KA, object2@KA)
    TH <- combineObject(object1@TH, object2@TH)
    RTH <- combineObject(object1@RTH, object2@RTH)
    Output <- new("MatrixSet", modelType = object1@modelType, LY = LY, VTE = VTE, TE = TE, RTE = RTE, VY = VY, TY = TY, MY = MY, BE = BE, 
        VPS = VPS, PS = PS, RPS = RPS, VE = VE, AL = AL, ME = ME, LX = LX, VTD = VTD, TD = TD, RTD = RTD, VX = VX, TX = TX, MX = MX, GA = GA, 
        VPH = VPH, PH = PH, RPH = RPH, KA = KA, TH = TH, RTH = RTH)
    return(Output)
})

setMethod("combineObject", signature(object1 = "SimParam", object2 = "list"), definition = function(object1, object2) {
    modelType <- object1@modelType
    exo <- (modelType == "SEM.exo") | (modelType == "Path.exo")
    nx <- ny <- ne <- nk <- 0
    if (modelType == "CFA") {
        ny <- nrow(object1@LY)
        ne <- nrow(object1@PS)
        name.indicator <- paste("y", 1:ny, sep = "")
        name.factor <- paste("e", 1:ne, sep = "")
        if (!isNullObject(object1@LY)) 
            object1@LY[is.na(object1@LY)] <- (object2$lambda[name.indicator, name.factor])[is.na(object1@LY)]
        if (!isNullObject(object1@PS)) 
            object1@PS[is.na(object1@PS)] <- (object2$psi[name.factor, name.factor])[is.na(object1@PS)]
        if (!isNullObject(object1@TE)) 
            object1@TE[is.na(object1@TE)] <- (object2$theta[name.indicator, name.indicator])[is.na(object1@TE)]
        if (!isNullObject(object1@TY)) 
            object1@TY[is.na(object1@TY)] <- (object2$nu[name.indicator, ])[is.na(object1@TY)]
        if (!isNullObject(object1@AL)) 
            object1@AL[is.na(object1@AL)] <- (object2$alpha[name.factor, ])[is.na(object1@AL)]
    } else if (modelType == "Path") {
        ny <- nrow(object1@PS)
        name.indicator <- paste("y", 1:ny, sep = "")
        if (!isNullObject(object1@PS)) 
            object1@PS[is.na(object1@PS)] <- (object2$psi[name.indicator, name.indicator])[is.na(object1@PS)]
        if (!isNullObject(object1@AL)) 
            object1@AL[is.na(object1@AL)] <- (object2$alpha[name.indicator, ])[is.na(object1@AL)]
        if (!isNullObject(object1@BE)) 
            object1@BE[is.na(object1@BE)] <- (object2$beta[name.indicator, name.indicator])[is.na(object1@BE)]
    } else if (modelType == "Path.exo") {
        ny <- nrow(object1@PS)
        nx <- nrow(object1@PH)
        name.indicator <- c(paste("x", 1:nx, sep = ""), paste("y", 1:ny, sep = ""))
        k.list <- 1:nx
        e.list <- (nx + 1):(nx + ny)
        if (!isNullObject(object1@PS)) 
            object1@PS[is.na(object1@PS)] <- ((object2$psi[name.indicator, name.indicator])[e.list, e.list])[is.na(object1@PS)]
        if (!isNullObject(object1@AL)) 
            object1@AL[is.na(object1@AL)] <- ((object2$alpha[name.indicator, ])[e.list])[is.na(object1@AL)]
        if (!isNullObject(object1@BE)) 
            object1@BE[is.na(object1@BE)] <- ((object2$beta[name.indicator, name.indicator])[e.list, e.list])[is.na(object1@BE)]
        if (!isNullObject(object1@PH)) 
            object1@PH[is.na(object1@PH)] <- ((object2$psi[name.indicator, name.indicator])[k.list, k.list])[is.na(object1@PH)]
        if (!isNullObject(object1@KA)) 
            object1@KA[is.na(object1@KA)] <- ((object2$alpha[name.indicator, ])[e.list])[is.na(object1@KA)]
        if (!isNullObject(object1@GA)) 
            object1@GA[is.na(object1@GA)] <- ((object2$beta[name.indicator, name.indicator])[e.list, k.list])[is.na(object1@GA)]
    } else if (modelType == "SEM") {
        ny <- nrow(object1@LY)
        ne <- nrow(object1@PS)
        name.indicator <- paste("y", 1:ny, sep = "")
        name.factor <- paste("e", 1:ne, sep = "")
        if (!isNullObject(object1@LY)) 
            object1@LY[is.na(object1@LY)] <- (object2$lambda[name.indicator, name.factor])[is.na(object1@LY)]
        if (!isNullObject(object1@PS)) 
            object1@PS[is.na(object1@PS)] <- (object2$psi[name.factor, name.factor])[is.na(object1@PS)]
        if (!isNullObject(object1@TE)) 
            object1@TE[is.na(object1@TE)] <- (object2$theta[name.indicator, name.indicator])[is.na(object1@TE)]
        if (!isNullObject(object1@TY)) 
            object1@TY[is.na(object1@TY)] <- (object2$nu[name.indicator, ])[is.na(object1@TY)]
        if (!isNullObject(object1@AL)) 
            object1@AL[is.na(object1@AL)] <- (object2$alpha[name.factor, ])[is.na(object1@AL)]
        if (!isNullObject(object1@BE)) 
            object1@BE[is.na(object1@BE)] <- (object2$beta[name.factor, name.factor])[is.na(object1@BE)]
    } else if (modelType == "SEM.exo") {
        ny <- nrow(object1@LY)
        nx <- nrow(object1@LX)
        ne <- nrow(object1@PS)
        nk <- nrow(object1@PH)
        name.indicator <- c(paste("x", 1:nx, sep = ""), paste("y", 1:ny, sep = ""))
        name.factor <- c(paste("k", 1:nk, sep = ""), paste("e", 1:ne, sep = ""))
        k.list <- 1:nk
        e.list <- (nk + 1):(nk + ne)
        if (!isNullObject(object1@PS)) 
            object1@PS[is.na(object1@PS)] <- ((object2$psi[name.factor, name.factor])[e.list, e.list])[is.na(object1@PS)]
        if (!isNullObject(object1@AL)) 
            object1@AL[is.na(object1@AL)] <- ((object2$alpha[name.factor, ])[e.list])[is.na(object1@AL)]
        if (!isNullObject(object1@BE)) 
            object1@BE[is.na(object1@BE)] <- ((object2$beta[name.factor, name.factor])[e.list, e.list])[is.na(object1@BE)]
        if (!isNullObject(object1@PH)) 
            object1@PH[is.na(object1@PH)] <- ((object2$psi[name.factor, name.factor])[k.list, k.list])[is.na(object1@PH)]
        if (!isNullObject(object1@KA)) 
            object1@KA[is.na(object1@KA)] <- ((object2$alpha[name.factor, ])[e.list])[is.na(object1@KA)]
        if (!isNullObject(object1@GA)) 
            object1@GA[is.na(object1@GA)] <- ((object2$beta[name.factor, name.factor])[e.list, k.list])[is.na(object1@GA)]
        x.list <- 1:nx
        y.list <- (nx + 1):(nx + ny)
        if (!isNullObject(object1@LY)) 
            object1@LY[is.na(object1@LY)] <- ((object2$lambda[name.indicator, name.factor])[y.list, e.list])[is.na(object1@LY)]
        if (!isNullObject(object1@TE)) 
            object1@TE[is.na(object1@TE)] <- ((object2$theta[name.indicator, name.indicator])[y.list, y.list])[is.na(object1@TE)]
        if (!isNullObject(object1@TY)) 
            object1@TY[is.na(object1@TY)] <- ((object2$nu[name.indicator, ])[y.list])[is.na(object1@TY)]
        if (!isNullObject(object1@LX)) 
            object1@LX[is.na(object1@LX)] <- ((object2$lambda[name.indicator, name.factor])[x.list, k.list])[is.na(object1@LX)]
        if (!isNullObject(object1@TD)) 
            object1@TD[is.na(object1@TD)] <- ((object2$theta[name.indicator, name.indicator])[x.list, x.list])[is.na(object1@TD)]
        if (!isNullObject(object1@TX)) 
            object1@TX[is.na(object1@TX)] <- ((object2$nu[name.indicator, ])[x.list])[is.na(object1@TX)]
        if (!isNullObject(object1@TH)) 
            object1@TH[is.na(object1@TH)] <- ((object2$theta[name.indicator, name.indicator])[x.list, y.list])[is.na(object1@TH)]
    } else {
        stop("something wrong!")
    }
    return(new("SimRSet", modelType = object1@modelType, PS = object1@PS, AL = object1@AL, BE = object1@BE, PH = object1@PH, KA = object1@KA, 
        GA = object1@GA, LY = object1@LY, TE = object1@TE, TY = object1@TY, LX = object1@LX, TD = object1@TD, TX = object1@TX, TH = object1@TH))
}) 
