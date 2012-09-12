# extract: Extract some elements from an object

setMethod("extract", signature = "SimDataDist", definition = function(object, pos) {
    return(new("SimDataDist", p = length(pos), dist = object@dist[pos], keepScale = object@keepScale[pos], reverse = object@reverse[pos]))
})

setMethod("extract", signature = "vector", definition = function(object, pos = NULL) {
    if (isNullObject(object)) 
        return(object)
    if (is.null(pos)) {
        return(object)
    } else {
        return(object[pos])
    }
})

setMethod("extract", signature = "matrix", definition = function(object, row = NULL, col = NULL) {
    if (isNullObject(object)) 
        return(object)
    if (is.null(row)) 
        row <- 1:nrow(object)
    if (is.null(col)) 
        col <- 1:ncol(object)
    if (length(row) > 1 & length(col) > 1) {
        return(object[row, col])
    } else {
        if (length(col) == 1) 
            return(as.matrix(object[row, col]))
        if (length(row) == 1) 
            return(t(as.matrix(object[row, col])))
    }
})

setMethod("extract", signature = "SimMatrix", definition = function(object, row = NULL, col = NULL) {
    if (isNullObject(object)) 
        return(object)
    if (is.null(row)) 
        row <- 1:nrow(object@free)
    if (is.null(col)) 
        col <- 1:ncol(object@free)
    object@free <- extract(object@free, row, col)
    object@value <- extract(object@value, row, col)
    return(object)
})

setMethod("extract", signature = "SimVector", definition = function(object, pos = NULL) {
    if (isNullObject(object)) 
        return(object)
    if (is.null(pos)) 
        pos <- 1:length(object@free)
    object@free <- object@free[pos]
    object@value <- object@value[pos]
    return(object)
})

setMethod("extract", signature = "SimSet", definition = function(object, yOnly = FALSE, y = NULL, e = NULL, x = NULL, k = NULL) {
    if (yOnly) {
        if (object@modelType == "CFA") 
            stop("The yOnly option can be used only for the object in path analysis or SEM model with X side.")
        if (!is.null(y) | !is.null(e) | !is.null(x) | !is.null(k)) 
            stop("The 'y', 'e', 'x', and 'k' arguments can be used only when the yOnly argument is FALSE.")
        object@modelType <- gsub(".exo", "", object@modelType)
        object@LX <- new("NullSimMatrix")
        object@RTD <- new("NullSymMatrix")
        object@VTD <- new("NullSimVector")
        object@RPH <- new("NullSymMatrix")
        object@GA <- new("NullSimMatrix")
        object@TX <- new("NullSimVector")
        object@KA <- new("NullSimVector")
        object@MX <- new("NullSimVector")
        object@VPH <- new("NullSimVector")
        object@VX <- new("NullSimVector")
        object@RTH <- new("NullSimMatrix")
    } else {
        if (object@modelType == "Path" | object@modelType == "Path.exo") {
            if (is.null(y)) 
                y <- 1:nrow(object@RPS@free)
            object@BE <- extract(object@BE, y, y)
            object@RPS <- extract(object@RPS, y, y)
            object@VPS <- extract(object@VPS, y)
            object@VE <- extract(object@VE, y)
            object@AL <- extract(object@AL, y)
            object@ME <- extract(object@ME, y)
            if (object@modelType == "Path.exo") {
                if (is.null(x)) 
                  x <- 1:nrow(object@RPH@free)
                object@GA <- extract(object@GA, y, x)
                object@RPH <- extract(object@RPH, x, x)
                object@VPH <- extract(object@VPH, x)
                object@KA <- extract(object@KA, x)
            }
        } else {
            if (is.null(y)) 
                y <- 1:nrow(object@LY@free)
            if (is.null(e)) 
                e <- 1:ncol(object@LY@free)
            object@LY <- extract(object@LY, y, e)
            object@RTE <- extract(object@RTE, y, y)
            object@VTE <- extract(object@VTE, y)
            object@VY <- extract(object@VY, y)
            object@TY <- extract(object@TY, y)
            object@MY <- extract(object@MY, y)
            object@BE <- extract(object@BE, e, e)
            object@RPS <- extract(object@RPS, e, e)
            object@VPS <- extract(object@VPS, e)
            object@VE <- extract(object@VE, e)
            object@AL <- extract(object@AL, e)
            object@ME <- extract(object@ME, e)
            if (object@modelType == "SEM.exo") {
                if (is.null(x)) 
                  x <- 1:nrow(object@LX@free)
                if (is.null(k)) 
                  k <- 1:ncol(object@LX@free)
                object@LX <- extract(object@LX, x, k)
                object@RTD <- extract(object@RTD, x, x)
                object@VTD <- extract(object@VTD, x)
                object@VX <- extract(object@VX, x)
                object@TX <- extract(object@TX, x)
                object@MX <- extract(object@MX, x)
                object@GA <- extract(object@GA, e, k)
                object@RPH <- extract(object@RPH, k, k)
                object@VPH <- extract(object@VPH, k)
                object@KA <- extract(object@KA, k)
                object@RTH <- extract(object@RTH, x, y)
            }
        }
    }
    return(object)
})

setMethod("extract", signature = "VirtualRSet", definition = function(object, yOnly = FALSE, y = NULL, e = NULL, x = NULL, 
    k = NULL) {
    if (yOnly) {
        if (object@modelType == "CFA") 
            stop("The yOnly option can be used only for the object in path analysis or SEM model with X side.")
        if (!is.null(y) | !is.null(e) | !is.null(x) | !is.null(k)) 
            stop("The 'y', 'e', 'x', and 'k' arguments can be used only when the yOnly argument is FALSE.")
        object@modelType <- gsub(".exo", "", object@modelType)
        object@LX <- new("NullMatrix")
        object@TD <- new("NullMatrix")
        object@PH <- new("NullMatrix")
        object@GA <- new("NullMatrix")
        object@TX <- new("NullVector")
        object@KA <- new("NullVector")
        object@TH <- new("NullMatrix")
    } else {
        if (object@modelType == "Path" | object@modelType == "Path.exo") {
            if (is.null(y)) 
                y <- 1:nrow(object@PS)
            object@BE <- extract(object@BE, y, y)
            object@PS <- extract(object@PS, y, y)
            object@AL <- extract(object@AL, y)
            if (object@modelType == "Path.exo") {
                if (is.null(x)) 
                  x <- 1:nrow(object@PH)
                object@GA <- extract(object@GA, y, x)
                object@PH <- extract(object@PH, x, x)
                object@KA <- extract(object@KA, x)
            }
        } else {
            if (is.null(y)) 
                y <- 1:nrow(object@LY)
            if (is.null(e)) 
                e <- 1:ncol(object@LY)
            object@LY <- extract(object@LY, y, e)
            object@TE <- extract(object@TE, y, y)
            object@TY <- extract(object@TY, y)
            object@BE <- extract(object@BE, e, e)
            object@PS <- extract(object@PS, e, e)
            object@AL <- extract(object@AL, e)
            if (object@modelType == "SEM.exo") {
                if (is.null(x)) 
                  x <- 1:nrow(object@LX)
                if (is.null(k)) 
                  k <- 1:ncol(object@LX)
                object@LX <- extract(object@LX, x, k)
                object@TD <- extract(object@TD, x, x)
                object@TX <- extract(object@TX, x)
                object@GA <- extract(object@GA, e, k)
                object@PH <- extract(object@PH, k, k)
                object@KA <- extract(object@KA, k)
                object@TH <- extract(object@TH, x, y)
            }
        }
    }
    return(object)
})

setMethod("extract", signature = "data.frame", definition = function(object, yOnly = FALSE, y = NULL, e = NULL, x = NULL, k = NULL, 
    keepOriginalName = FALSE) {
    columnName <- colnames(object)
    if (is.null(columnName)) 
        stop("The extract method for data frame needs column names.")
    name <- substr(columnName, 1, 2)
    position <- do.call("rbind", strsplit(substr(columnName, 3, nchar(columnName)), "_"))
    if (yOnly) {
        if (!is.null(y) | !is.null(e) | !is.null(x) | !is.null(k)) 
            stop("The 'y', 'e', 'x', and 'k' arguments can be used only when the yOnly argument is FALSE.")
        object <- object[, which(name %in% c("PS", "LY", "TY", "TE", "AL", "BE"))]
    } else {
        modelType <- NULL
        if ((length(grep("LY", name)) > 0) | (length(grep("LX", name)) > 0)) {
            modelType <- "CFA"
            if (length(grep("BE", name)) > 0) 
                modelType <- "SEM"
            if (length(grep("LX", name)) > 0) 
                modelType <- "SEM.exo"
        } else {
            modelType <- "Path"
            if (length(grep("GA", name)) > 0) 
                modelType <- "Path.exo"
        }
        resultName <- NULL
        newName <- NULL
        if (modelType == "Path" | modelType == "Path.exo") {
            if (length(columnName[name == "BE"]) > 0) {
                resultBE <- extractMatrixNames(columnName[name == "BE"], y, y)
                resultName <- c(resultName, resultBE[[1]])
                newName <- c(newName, resultBE[[2]])
            }
            if (length(columnName[name == "PS"]) > 0) {
                resultPS <- extractMatrixNames(columnName[name == "PS"], y, y)
                resultName <- c(resultName, resultPS[[1]])
                newName <- c(newName, resultPS[[2]])
            }
            if (length(columnName[name == "AL"]) > 0) {
                resultAL <- extractVectorNames(columnName[name == "AL"], y)
                resultName <- c(resultName, resultAL[[1]])
                newName <- c(newName, resultAL[[2]])
            }
            if (length(columnName[name == "GA"]) > 0) {
                resultGA <- extractMatrixNames(columnName[name == "GA"], y, x)
                resultName <- c(resultName, resultGA[[1]])
                newName <- c(newName, resultGA[[2]])
            }
            if (length(columnName[name == "PH"]) > 0) {
                resultPH <- extractMatrixNames(columnName[name == "PH"], x, x)
                resultName <- c(resultName, resultPH[[1]])
                newName <- c(newName, resultPH[[2]])
            }
            if (length(columnName[name == "KA"]) > 0) {
                resultKA <- extractVectorNames(columnName[name == "KA"], x)
                resultName <- c(resultName, resultKA[[1]])
                newName <- c(newName, resultAL[[2]])
            }
        } else {
            if (length(columnName[name == "BE"]) > 0) {
                resultBE <- extractMatrixNames(columnName[name == "BE"], e, e)
                resultName <- c(resultName, resultBE[[1]])
                newName <- c(newName, resultBE[[2]])
            }
            if (length(columnName[name == "PS"]) > 0) {
                resultPS <- extractMatrixNames(columnName[name == "PS"], e, e)
                resultName <- c(resultName, resultPS[[1]])
                newName <- c(newName, resultPS[[2]])
            }
            if (length(columnName[name == "AL"]) > 0) {
                resultAL <- extractVectorNames(columnName[name == "AL"], e)
                resultName <- c(resultName, resultAL[[1]])
                newName <- c(newName, resultAL[[2]])
            }
            if (length(columnName[name == "GA"]) > 0) {
                resultGA <- extractMatrixNames(columnName[name == "GA"], e, k)
                resultName <- c(resultName, resultGA[[1]])
                newName <- c(newName, resultGA[[2]])
            }
            if (length(columnName[name == "PH"]) > 0) {
                resultPH <- extractMatrixNames(columnName[name == "PH"], k, k)
                resultName <- c(resultName, resultPH[[1]])
                newName <- c(newName, resultPH[[2]])
            }
            if (length(columnName[name == "KA"]) > 0) {
                resultKA <- extractVectorNames(columnName[name == "KA"], k)
                resultName <- c(resultName, resultKA[[1]])
                newName <- c(newName, resultAL[[2]])
            }
            if (length(columnName[name == "LY"]) > 0) {
                resultLY <- extractMatrixNames(columnName[name == "LY"], y, e)
                resultName <- c(resultName, resultLY[[1]])
                newName <- c(newName, resultLY[[2]])
            }
            if (length(columnName[name == "TE"]) > 0) {
                resultTE <- extractMatrixNames(columnName[name == "TE"], y, y)
                resultName <- c(resultName, resultTE[[1]])
                newName <- c(newName, resultTE[[2]])
            }
            if (length(columnName[name == "TY"]) > 0) {
                resultTY <- extractVectorNames(columnName[name == "TY"], y)
                resultName <- c(resultName, resultTY[[1]])
                newName <- c(newName, resultTY[[2]])
            }
            if (length(columnName[name == "LX"]) > 0) {
                resultLX <- extractMatrixNames(columnName[name == "LX"], x, k)
                resultName <- c(resultName, resultLX[[1]])
                newName <- c(newName, resultLX[[2]])
            }
            if (length(columnName[name == "TD"]) > 0) {
                resultTD <- extractMatrixNames(columnName[name == "TD"], x, x)
                resultName <- c(resultName, resultTD[[1]])
                newName <- c(newName, resultTD[[2]])
            }
            if (length(columnName[name == "TX"]) > 0) {
                resultTX <- extractVectorNames(columnName[name == "TX"], x)
                resultName <- c(resultName, resultTX[[1]])
                newName <- c(newName, resultTX[[2]])
            }
            if (length(columnName[name == "TH"]) > 0) {
                resultTH <- extractMatrixNames(columnName[name == "TH"], x, y)
                resultName <- c(resultName, resultTH[[1]])
                newName <- c(newName, resultTH[[2]])
            }
        }
        newOrder <- resultName[order(match(resultName, columnName))]
        object <- object[, newOrder]
        if (keepOriginalName == FALSE) {
            colnames(object) <- newName[order(match(resultName, newOrder))]
        }
    }
    return(object)
}) 
