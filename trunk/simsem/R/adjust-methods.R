# adjust: adjust a particular element in an object

setMethod("adjust", signature(target = "SimMatrix"), definition = function(target, 
    param, pos, numAsFixed = TRUE) {
    if (is.vector(pos) && (length(pos) == 2)) 
        pos <- matrix(pos, ncol = 2)
    for (i in 1:nrow(pos)) {
        if (is.character(param)) {
            target@param[pos[i, 1], pos[i, 2]] <- param
            target@free[pos[i, 1], pos[i, 2]] <- NA
        } else if (is.numeric(param)) {
            if (numAsFixed) {
                target@param[pos[i, 1], pos[i, 2]] <- ""
                target@free[pos[i, 1], pos[i, 2]] <- param
            } else {
                target@param[pos[i, 1], pos[i, 2]] <- as.character(param)
                target@free[pos[i, 1], pos[i, 2]] <- NA
            }
        } else {
            stop("Please put a number or a name of random distribution object to the param attribute")
        }
    }
    return(target)  #new('SimMatrix', free=target@free, param=target@param))
})

setMethod("adjust", signature(target = "SymMatrix"), definition = function(target, 
    param, pos, numAsFixed = TRUE) {
    if (is.vector(pos) && (length(pos) == 2)) 
        pos <- matrix(pos, ncol = 2)
    for (i in 1:nrow(pos)) {
        if (is.character(param)) {
            target@param[pos[i, 1], pos[i, 2]] <- param
            target@free[pos[i, 1], pos[i, 2]] <- NA
            target@param[pos[i, 2], pos[i, 1]] <- param
            target@free[pos[i, 2], pos[i, 1]] <- NA
        } else if (is.numeric(param)) {
            if (numAsFixed) {
                target@param[pos[i, 1], pos[i, 2]] <- ""
                target@free[pos[i, 1], pos[i, 2]] <- param
                target@param[pos[i, 2], pos[i, 1]] <- ""
                target@free[pos[i, 2], pos[i, 1]] <- param
            } else {
                target@param[pos[i, 1], pos[i, 2]] <- as.character(param)
                target@free[pos[i, 1], pos[i, 2]] <- NA
                target@param[pos[i, 2], pos[i, 1]] <- as.character(param)
                target@free[pos[i, 2], pos[i, 1]] <- NA
            }
        } else {
            stop("Please put a number or a name of random distribution object to the param attribute")
        }
    }
    return(target)  #new('SimMatrix', free=target@free, param=target@param))
})

setMethod("adjust", signature(target = "SimVector"), definition = function(target, 
    param, pos, numAsFixed = TRUE) {
    for (i in 1:length(pos)) {
        if (is.character(param)) {
            target@param[pos[i]] <- param
            target@free[pos[i]] <- NA
        } else if (is.numeric(param)) {
            if (numAsFixed) {
                target@param[pos[i]] <- ""
                target@free[pos[i]] <- param
            } else {
                target@param[pos[i]] <- as.character(param)
                target@free[pos[i]] <- NA
            }
        } else {
            stop("Please put a number or a name of random distribution object to the param attribute")
        }
    }
    return(target)
}) 
