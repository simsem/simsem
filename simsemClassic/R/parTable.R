# Builds a parameter table.  paramSet - list of simMatrix constraint - constraint object: SimEqualCon aux = names of the index of
# the auxiliary variables in the data

# To get code to work a <- models(4) a.set <- simSetSEM(LY=a$LY,RPS=a$RPS,RTE=a$RTE,BE=a$BE) paramSet <-
# list(LY=a$LY,RPS=a$RPS,RTE=a$RTE,BE=a$BE) ad <- simData(a.set) acm <- simModel(a.set,a$con) param <- tagHeaders(acm@param)

# runLavaan(acm,run(ad,200))

# The necessary steps to building the analysis model: 1. Check to see if user specification is valid (current simSet) 2. Reduce
# correlation matrices to covariance / Transform any X-side notation to Y-Side 3. Calculate starting values (?)  4. Determine free
# parameters 5. make labels 6. impose constraints 7. Specify package 8. Estimator 9. Specify auxiliary variables (?)  Final data
# type: List with [[1]] -> par Table (df) [[2]] -> package (char) [[3]] -> estimator (char) [[4]] -> Auxiliary (v)


# HS.model <- 'f1 =~ x1 + x2 + x3 \n f2 =~ x4 + x5 +x6 \n f3 =~ x7 + x8 + x9' fit <- cfa(HS.model, data=HolzingerSwineford1939)
# parTable(fit)

# Lets just leave the constraints and aux for now, and plan on it being a list of simMatrix.

buildPT <- function(paramSet, modelType) {
    paramSet <- getFree(paramSet)
	aux <- NULL # SP: put it here first to get the package compilable
    # con <- reduceConstraint(constraint)
    
    # This is repeated in simModel? B
    
    paramSet <- collapseExo(paramSet, label = TRUE) 
	constraint <- collapseExo(constraint, label = TRUE, value = NA)
    ###################Have some zeros
    if (!isNullObject(paramSet$LY)) {
        for (i in 1:ncol(paramSet$LY)) {
            temp <- paste(colnames(paramSet$LY)[i], "=~")
            something <- FALSE
            for (j in 1:nrow(paramSet$LY)) {
                if (is.na(paramSet$LY[j, i]) | paramSet$LY[j, i] != 0 | !is.na(constraint@LY[j, i])) {
                  content <- paste(paramSet$LY[j, i], "*", sep = "")
                  if (!is.na(constraint@LY[j, i])) 
                    content <- constraint@LY[j, i]
                  temp <- paste(temp, " ", content, rownames(paramSet$LY)[j], sep = "")
                  something <- TRUE
                }
                if (something && j != nrow(paramSet$LY) && (is.na(paramSet$LY[j + 1, i]) | paramSet$LY[j + 1, i] != 0)) 
                  temp <- paste(temp, "+")
            }
            if ((sum(is.na(paramSet$LY[, i])) == 0) && (sum(paramSet$LY[, i]) == 0)) 
                temp <- paste(temp, "0*", rownames(paramSet$LY)[1], sep = "")
            if (!is.null(aux)) {
                noVar <- !(is.na(diag(paramSet$TE)) | diag(paramSet$TE) != 0)
                lab <- "0"
                if (sum(noVar) > 0) {
                  LYsingle <- extract(paramSet$LY, which(noVar), i)
                  LYsingle <- is.na(LYsingle) | LYsingle != 0
                  if (sum(LYsingle) > 0) 
                    lab <- "NA"
                }
                temp <- paste(temp, paste(paste(" + ", lab, "*", aux, sep = ""), collapse = ""))
            }
            result <- paste(result, temp, "\n")
        }
    }
    if (!isNullObject(paramSet$BE)) {
        for (i in 1:nrow(paramSet$BE)) {
            temp <- NULL
            for (j in 1:ncol(paramSet$BE)) {
                if (is.na(paramSet$BE[i, j]) | paramSet$BE[i, j] != 0 | !is.na(constraint@BE[i, j])) {
                  content <- paste(paramSet$BE[i, j], "*", sep = "")
                  if (!is.na(constraint@BE[i, j])) 
                    content <- constraint@BE[i, j]
                  temp <- paste(temp, " ", content, colnames(paramSet$BE)[j], sep = "")
                }
                if (!is.null(temp) && j != ncol(paramSet$BE) && (is.na(paramSet$BE[i, j + 1]) | paramSet$BE[i, j + 1] != 0)) 
                  temp <- paste(temp, "+")
            }
            if (!is.null(temp)) {
                temp2 <- paste(rownames(paramSet$BE)[i], "~")
                result <- paste(result, temp2, temp, "\n")
            }
        }
        if (isNullObject(paramSet$LY) && !is.null(aux)) {
            set <- findRecursiveSet(paramSet$BE)
            target <- colnames(paramSet$BE)[set[[1]]]
            for (i in 1:length(aux)) {
                temp <- paste(aux[1], " ~ ", paste(paste("NA*", target), collapse = " + "), "\n", sep = "")
                result <- paste(result, temp)
            }
        }
    }
    if (!isNullObject(paramSet$PS)) {
        var.code <- NULL
        for (i in 1:length(diag(paramSet$PS))) {
            if (!is.na(paramSet$PS[i, i]) | !is.na(constraint@PS[i, i])) {
                content <- paste(paramSet$PS[i, i], "*", sep = "")
                if (!is.na(constraint@PS[i, i])) 
                  content <- constraint@PS[i, i]
                var.code <- paste(var.code, colnames(paramSet$PS)[i], " ~~ ", content, colnames(paramSet$PS)[i], " \n", sep = "")
            }
        }
        cov.code <- NULL
        if (nrow(paramSet$PS) > 1) {
            for (i in 2:nrow(paramSet$PS)) {
                for (j in 1:(i - 1)) {
                  if (is.na(paramSet$PS[i, j]) | paramSet$PS[i, j] != 0 | !is.na(constraint@PS[i, j])) {
                    content <- paste(paramSet$PS[i, j], "*", sep = "")
                    if (!is.na(constraint@PS[i, j])) 
                      content <- constraint@PS[i, j]
                    if (isNullObject(paramSet$BE)) {
                      cov.code <- paste(cov.code, rownames(paramSet$PS)[i], " ~~ ", content, colnames(paramSet$PS)[j], " \n", sep = "")
                    } else {
                      exo.set <- findRecursiveSet(paramSet$BE)[[1]]
                      if (!(is.element(i, exo.set) & is.element(j, exo.set))) {
                        cov.code <- paste(cov.code, rownames(paramSet$PS)[i], " ~~ ", content, colnames(paramSet$PS)[j], " \n", sep = "")
                      }
                    }
                  } else {
                    content <- paste(paramSet$PS[i, j], "*", sep = "")
                    if (isNullObject(paramSet$BE)) {
                      cov.code <- paste(cov.code, rownames(paramSet$PS)[i], " ~~ ", content, colnames(paramSet$PS)[j], " \n", sep = "")
                    } else {
                      auxFac <- which(apply(paramSet$BE, 1, function(x) all(!is.na(x) & (x == 0))) & apply(paramSet$BE, 2, function(x) all(!is.na(x) & 
                        (x == 0))))
                      if (is.element(i, auxFac) | is.element(j, auxFac)) {
                        cov.code <- paste(cov.code, rownames(paramSet$PS)[i], " ~~ 0*", colnames(paramSet$PS)[j], " \n", sep = "")
                      }
                    }
                  }
                }
            }
        }
        result <- paste(result, var.code, cov.code)
        if (isNullObject(paramSet$LY) && !is.null(aux)) {
            set <- findRecursiveSet(paramSet$BE)
            target <- colnames(paramSet$BE)[-set[[1]]]
            varCode <- paste(paste(aux, " ~~ NA*", aux, sep = ""), collapse = "\n")
            result <- paste(result, varCode, "\n")
            corCode <- paste(outer(aux, target, paste, sep = " ~~ NA*"), collapse = "\n")
            result <- paste(result, corCode, "\n")
        }
    }
    if (!isNullObject(paramSet$TE)) {
        var.code <- NULL
        for (i in 1:length(diag(paramSet$TE))) {
            if (!is.na(paramSet$TE[i, i]) | !is.na(constraint@TE[i, i])) {
                content <- paste(paramSet$TE[i, i], "*", sep = "")
                if (!is.na(constraint@TE[i, i])) 
                  content <- constraint@TE[i, i]
                var.code <- paste(var.code, colnames(paramSet$TE)[i], " ~~ ", content, colnames(paramSet$TE)[i], " \n", sep = "")
            }
        }
        cov.code <- NULL
        for (i in 2:nrow(paramSet$TE)) {
            for (j in 1:(i - 1)) {
                if (is.na(paramSet$TE[i, j]) | paramSet$TE[i, j] != 0 | !is.na(constraint@TE[i, j])) {
                  content <- paste(paramSet$TE[i, j], "*", sep = "")
                  if (!is.na(constraint@TE[i, j])) 
                    content <- constraint@TE[i, j]
                  cov.code <- paste(cov.code, rownames(paramSet$TE)[i], " ~~ ", content, colnames(paramSet$TE)[j], " \n", sep = "")
                }
            }
        }
        result <- paste(result, var.code, cov.code)
        if (!isNullObject(paramSet$LY) && !is.null(aux)) {
            nonConstant <- is.na(diag(paramSet$TE)) | diag(paramSet$TE) != 0
            target <- colnames(paramSet$TE)[nonConstant]
            varCode <- paste(paste(aux, " ~~ NA*", aux, sep = ""), collapse = "\n")
            result <- paste(result, varCode, "\n")
            corCode <- paste(outer(aux, target, paste, sep = " ~~ NA*"), collapse = "\n")
            result <- paste(result, corCode, "\n")
        }
    }
    if (!is.null(aux) && length(aux) > 1) {
        corCode <- outer(aux, aux, paste, sep = " ~~ NA*")
        diag(corCode) <- ""
        corCode <- corCode[lower.tri(corCode)]
        corCode <- paste(paste(corCode, collapse = "\n"), "\n")
        result <- paste(result, corCode)
    }
    if (!isNullObject(paramSet$AL)) {
        mean.code <- NULL
        for (i in 1:length(paramSet$AL)) {
            if (!(paramSet$modelType == "Path" | paramSet$modelType == "Path.exo") | !is.na(paramSet$AL[i]) | !is.na(constraint@AL[i])) {
                content <- paste(paramSet$AL[i], "*", sep = "")
                if (!is.na(constraint@AL[i])) 
                  content <- constraint@AL[i]
                mean.code <- paste(mean.code, names(paramSet$AL)[i], " ~ ", content, "1 \n", sep = "")
            }
        }
        result <- paste(result, mean.code)
    }
    if (!isNullObject(paramSet$TY)) {
        mean.code <- NULL
        for (i in 1:length(paramSet$TY)) {
            content <- paste(paramSet$TY[i], "*", sep = "")
            if (!is.na(constraint@TY[i])) 
                content <- constraint@TY[i]
            mean.code <- paste(mean.code, names(paramSet$TY)[i], " ~ ", content, "1 \n", sep = "")
        }
        result <- paste(result, mean.code)
    }
    if (!is.null(aux)) {
        temp <- paste(paste(aux, " ~ NA*1 \n"), collapse = "")
        result <- paste(result, temp)
    }
    return(result)
}

writeLavaanConstraint2 <- function(object, constraint) {
    object <- blankParameters(object)
    if (!is.null(constraint)) {
        con <- constraint@con
        for (i in 1:length(con)) {
            current <- con[[i]]
            con.text <- writeLavaanIndividualConstraint(rownames(current)[1], current[1, ], slot(object, rownames(current)[1]))
            for (j in 2:nrow(current)) {
                Matrix <- rownames(current)[j]
                if (Matrix == "PS" | Matrix == "PH" | Matrix == "TE" | Matrix == "TD") {
                  elements <- c(as.numeric(current[j, 2]), as.numeric(current[j, 3]))
                  slot(object, Matrix)[max(elements), min(elements)] <- con.text
                } else {
                  slot(object, Matrix)[as.numeric(current[j, 2]), as.numeric(current[j, 3])] <- con.text
                }
            }
        }
    }
    return(object)
}

writeLavaanIndividualConstraint2 <- function(Matrix, Attribute, Names) { # SP: Get rid of redundant name of functions
    result <- "equal('"
    if (!is.na(Attribute[1])) 
        result <- paste(result, Attribute[1], ".", sep = "")
    if (length(Attribute) == 2) {
        result <- paste(result, names(Names)[as.numeric(Attribute[2])], " ~ 1')*", sep = "")
    } else if (length(Attribute) == 3) {
        Row <- as.numeric(Attribute[2])
        Column <- as.numeric(Attribute[3])
        if (Matrix == "LY" | Matrix == "LX") {
            result <- paste(result, colnames(Names)[Column], " =~ ", rownames(Names)[Row], "')*", sep = "")
        } else if (Matrix == "PS" | Matrix == "PH" | Matrix == "TE" | Matrix == "TD" | Matrix == "TH") {
            result <- paste(result, rownames(Names)[Row], " ~~ ", colnames(Names)[Column], "')*", sep = "")
        } else if (Matrix == "GA" | Matrix == "BE") {
            result <- paste(result, rownames(Names)[Row], " ~ ", colnames(Names)[Column], "')*", sep = "")
        }
    }
    return(result)
}

writeLavaanNullCode2 <- function(var, aux = NULL) { # SP: Get rid of redundant name of functions
    result <- NULL
    varAll <- c(var, aux)
    varCode <- paste(paste(paste(varAll, " ~~ NA*", varAll, sep = ""), collapse = "\n"), "\n")
    corCode <- outer(var, var, paste, sep = " ~~ 0*")
    diag(corCode) <- ""
    corCode <- corCode[lower.tri(corCode)]
    corCode <- paste(paste(corCode, collapse = "\n"), "\n")
    result <- paste(varCode, corCode)
    if (!is.null(aux)) {
        if (length(aux) > 1) {
            corCode2 <- outer(aux, aux, paste, sep = " ~~ NA*")
            diag(corCode2) <- ""
            corCode2 <- corCode2[lower.tri(corCode2)]
            corCode2 <- paste(paste(corCode2, collapse = "\n"), "\n")
            result <- paste(result, corCode2)
        }
        corCode3 <- paste(outer(aux, var, paste, sep = " ~~ NA*"), collapse = "\n")
        result <- paste(result, corCode3, "\n")
    }
    return(result)
}

reduceConstraint2 <- function(SimEqualCon) { # SP: Get rid of redundant name of functions
    modelType <- SimEqualCon@modelType
    equalCon <- SimEqualCon@con
    Length <- length(equalCon)
    Result <- NULL
    runnum <- 1
    for (i in 1:Length) {
        temp.result <- NULL
        temp.matrix <- equalCon[[i]]
        name <- rownames(temp.matrix)
        if (isMeanConstraint(name)) {
            if (sum(!is.element(name, c("ME", "MX", "MY"))) > 0) 
                temp.result <- temp.matrix
        } else if (isVarianceConstraint(name)) {
            if (sum(is.element(name, c("VE", "VX", "VY"))) > 0) {
                temp.result <- matrix(0, nrow(temp.matrix), 3)
                temp.result[, 1] <- temp.matrix[, 1]
                temp.result[, 2] <- temp.matrix[, 2]
                temp.result[, 3] <- temp.matrix[, 2]
                for (j in 1:length(name)) {
                  if (name[j] == "VTD") 
                    name[j] == "TD"
                  if (name[j] == "RTD") 
                    name[j] == "TD"
                  if (name[j] == "VTE") 
                    name[j] == "TE"
                  if (name[j] == "RTE") 
                    name[j] == "TE"
                  if (name[j] == "VPH") 
                    name[j] == "PH"
                  if (name[j] == "RPH") 
                    name[j] == "PH"
                  if (name[j] == "VPS") 
                    name[j] == "PS"
                  if (name[j] == "RPS") 
                    name[j] == "PS"
                }
            }
        } else {
            temp.result <- temp.matrix
        }
        if (!is.null(temp.result)) {
            Result[[runnum]] <- as.matrix(temp.result)
            runnum <- runnum + 1
        }
    }
    if (is.null(Result)) 
        Result <- list(NULL)
    return(new("SimREqualCon", con = Result, modelType = SimEqualCon@modelType))
    # return(Result)
}

isVarianceConstraint2 <- function(Name) { # SP: Get rid of redundant name of functions
    W <- getKeywords()
    keywords <- c(W$VTE, W$VTD, W$VPH, W$VPS, W$VX, W$VY, W$VE)
    result <- Name %in% keywords
    if (sum(result) == length(Name)) {
        return(TRUE)
    } else if (sum(result) == 0) {
        return(FALSE)
    } else {
        stop("A constraint matrix was mixed between variance and other types of elements.")
    }
}

isMeanConstraint2 <- function(Name) { # SP: Get rid of redundant name of functions
    W <- getKeywords()
    keywords <- c(W$TX, W$TY, W$KA, W$AL, W$MX, W$MY, W$ME)
    result <- Name %in% keywords
    if (sum(result) == length(Name)) {
        return(TRUE)
    } else if (sum(result) == 0) {
        return(FALSE)
    } else {
        stop("A constraint matrix was mixed between mean and other types of elements.")
    }
}

# Takes an arbitary list of SimMatrix and returns the free parameter matrices of that same list
getFree <- function(...) {
    mats <- unlist(list(...))
    return(lapply(mats, function(obj) {
        return(obj@free)
    }))
}

# Takes a list of free parameter matrices and the model type and labels the rows and columns
paramLabels <- function(paramSet, modelType) {
    ny <- NULL
    nx <- NULL
    nk <- NULL
    ne <- NULL
    
    if (modelType == "CFA") {
        ne <- ncol(paramSet$LY)
        ny <- nrow(paramSet$LY)
    } else if (modelType == "Path") {
        ny <- nrow(paramSet$PS)
    } else if (modelType == "Path.exo") {
        nx <- ncol(paramSet$GA)
        ny <- nrow(paramSet$PS)
    } else if (modelType == "SEM") {
        ne <- ncol(paramSet$LY)
        ny <- nrow(paramSet$LY)
    } else if (modelType == "SEM.exo") {
        ne <- ncol(paramSet$LY)
        ny <- nrow(paramSet$LY)
        nk <- ncol(paramSet$LX)
        nx <- nrow(paramSet$LX)
    }
    names.y <- NULL
    names.x <- NULL
    names.e <- NULL
    names.k <- NULL
    if (!is.null(ny)) {
        for (i in 1:ny) {
            temp <- paste("y", i, sep = "")
            names.y <- c(names.y, temp)
        }
    }
    if (!is.null(nx)) {
        for (i in 1:nx) {
            temp <- paste("x", i, sep = "")
            names.x <- c(names.x, temp)
        }
    }
    if (!is.null(ne)) {
        for (i in 1:ne) {
            temp <- paste("e", i, sep = "")
            names.e <- c(names.e, temp)
        }
    }
    if (!is.null(nk)) {
        for (i in 1:nk) {
            temp <- paste("k", i, sep = "")
            names.k <- c(names.k, temp)
        }
    }
    if (!is.null(paramSet$LY)) {
        colnames(paramSet$LY) <- names.e
        rownames(paramSet$LY) <- names.y
    }
    if (!is.null(paramSet$TE)) {
        colnames(paramSet$TE) <- names.y
        rownames(paramSet$TE) <- names.y
    }
    if (!is.null(paramSet$PS)) {
        if (modelType == "Path" | modelType == "Path.exo") {
            colnames(paramSet$PS) <- names.y
            rownames(paramSet$PS) <- names.y
        } else {
            colnames(paramSet$PS) <- names.e
            rownames(paramSet$PS) <- names.e
        }
    }
    if (!is.null(paramSet$BE)) {
        if (modelType == "Path" | modelType == "Path.exo") {
            colnames(paramSet$BE) <- names.y
            rownames(paramSet$BE) <- names.y
        } else {
            colnames(paramSet$BE) <- names.e
            rownames(paramSet$BE) <- names.e
        }
    }
    if (!is.null(paramSet$TY)) {
        names(paramSet$TY) <- names.y
    }
    if (!is.null(paramSet$AL)) {
        if (modelType == "Path" | modelType == "Path.exo") {
            names(paramSet$AL) <- names.y
        } else {
            names(paramSet$AL) <- names.e
        }
    }
    if (!is.null(paramSet$LX)) {
        colnames(paramSet$LX) <- names.k
        rownames(paramSet$LX) <- names.x
    }
    if (!is.null(paramSet$TD)) {
        colnames(paramSet$TD) <- names.x
        rownames(paramSet$TD) <- names.x
    }
    if (!is.null(paramSet$PH)) {
        if (modelType == "Path" | modelType == "Path.exo") {
            colnames(paramSet$PH) <- names.x
            rownames(paramSet$PH) <- names.x
        } else {
            colnames(paramSet$PH) <- names.k
            rownames(paramSet$PH) <- names.k
        }
    }
    if (!is.null(paramSet$GA)) {
        if (modelType == "Path" | modelType == "Path.exo") {
            colnames(paramSet$GA) <- names.x
            rownames(paramSet$GA) <- names.y
        } else {
            colnames(paramSet$GA) <- names.k
            rownames(paramSet$GA) <- names.e
        }
    }
    if (!is.null(paramSet$TX)) {
        names(paramSet$TX) <- names.x
    }
    if (!is.null(paramSet$KA)) {
        if (modelType == "Path" | modelType == "Path.exo") {
            names(paramSet$KA) <- names.x
        } else {
            names(paramSet$KA) <- names.k
        }
    }
    if (!is.null(paramSet$TH)) {
        colnames(paramSet$TH) <- names.y
        rownames(paramSet$TH) <- names.x
    }
    return(paramSet)
}

startValues <- function(paramSet, trial, reduced = FALSE) {
    result <- run(paramSet)
    if (trial > 1) {
        for (i in 2:trial) {
            temp <- run(paramSet)
            result <- combineObject(result, temp)
        }
        result <- divideObject(result, trial)
    }
    result@modelType <- paramSet@modelType
    if (reduced == TRUE) 
        result <- reduceMatrices(result)
    return(result)
} 
