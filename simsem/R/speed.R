library(simsem)

s <- function(dir) {
  sourceDir <- function(path, trace = TRUE, ...) {
     for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
		if(nm != "AllClass.R" & nm != "AllGenerics.R") {
        if(trace) cat(nm,":") 
        source(file.path(path, nm), ...)
        if(trace) cat("\n")
		}
     }
   }
 source(paste(dir, "AllClass.R", sep=""))
 source(paste(dir, "AllGenerics.R", sep=""))
 sourceDir(dir)
}

dir <- "/nfs/home/patr1ckm/repos/simsem/simsem/R/"

a <- system.time(replicate(1000,run(simUnif(0.3,0.5))))
b <- system.time(replicate(1000,eval(parse(text="runif(1,0.3,0.5)"))))
c <- system.time(replicate(1000,runif(1,0.3,0.5)))
d <- system.time(replicate(1000,eval(expression(runif(1,0.3,0.5)))))


##


loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LY <- simMatrix(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- symMatrix(latent.cor, 0.5)

error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
RTE <- symMatrix(error.cor)

CFA.Model <- simSetCFA(LY = LY, RPS = RPS, RTE = RTE)

SimData <- simData(CFA.Model, 200)

data <- run(SimData)

SimModel <- simModel(CFA.Model)

##

t1 <- system.time(a <- simResult(100, SimData, SimModel))
t2 <- system.time(b <- simResultB(5, SimData, SimModel,flag=TRUE))
t3 <- system.time(c <- simResultB(5, SimData, SimModel,flag=FALSE))
ex <- function() {simResultB(5, SimData, SimModel, flag=TRUE)}

#code <- " e1 =~ NA*y1 + NA*y2 + NA*y3 \n e2 =~ NA*y4 + NA*y5 + NA*y6 \n e1 ~~ 1*e1 \ne2 ~~ 1*e2 \n e2 ~~ NA*e1 \n
# e1 ~ 0*1 \ne2 ~ 0*1 \n y1 ~ NA*1 \ny2 ~ NA*1 \ny3 ~ NA*1 \ny4 ~ NA*1 \ny5 ~ NA*1 \ny6 ~ NA*1 \n"
#pt <- lavaanify(code)

load("/nfs/home/patr1ckm/R/MuckingAround/pt.Rdata")
pt <- a


## Let's get the simResult Chain in here and start messing around with it - reducing haphazardly to the necessities because we can and because it would be helpful.

    
simResultB <- function(nRep = NULL, objData = NULL, objModel = NULL, objMissing = new("NullSimMissing"), seed = 123321, silent = FALSE, multicore = FALSE, cluster = FALSE, numProc = NULL,  n = NULL, pmMCAR = NULL, pmMAR = NULL, objSet = NULL, objFunction = new("NullSimFunction"),flag) {

  if(flag) cat("pt\n") else cat("writeSyntax\n")

    if (!require(lavaan)) {
        install.packages("lavaan")
        tryCatch(library(lavaan), error = function(e) {
            stop("The lavaan package cannot be loaded. Please install lavaan packages manually.")
        })
    }
    set.seed(seed)
    warnT <- as.numeric(options("warn"))
    modelType <- objModel@modelType
    param <- NULL
    object.l <- list()
   
    if (class(objData) == "SimData") {
       fixedParam <- drawParameters(objData)
            for (i in 1:nRep) {
                object.l[[i]] <- fixedParam
            }
     }
    
    numseed <- as.list(round(sample(1:999999, nRep)))
    
    simParams <- list()
    for (i in 1:length(object.l)) { # actually 1:nRep
        simParams[[i]] <- list()
        simParams[[i]][[1]] <- object.l[[i]]
        simParams[[i]][[2]] <- n[i]
        simParams[[i]][[3]] <- pmMCAR[i]
        simParams[[i]][[4]] <- pmMAR[i]
        simParams[[i]][[5]] <- numseed[[i]]
    }
    
    Result.l <- lapply(simParams, runRep2, objData = objData, objModel = objModel, objMissing = objMissing, objFunction = objFunction, silent = silent, flag=flag)      
    
    Result <- combineReps(Result.l,nRep,seed,n,pmMAR,pmMCAR)
  
    if (silent) 
        options(warn = warnT)
    return <- Result
}

runRep2 <- function(simParams, objData, objModel, objMissing = new("NullSimMissing"), objFunction = new("NullSimFunction"), silent = FALSE, flag=flag) {
    modelType <- objModel@modelType
    param <- objModel@param
    coef <- NA
    se <- NA
    fit <- NA
    std <- NA
    FMI1 <- NULL
    FMI2 <- NULL
    converged <- FALSE
    seed <- simParams[[5]]
    n <- simParams[[2]]
    if (is.null(n)) 
        n <- objData@n
    pmMCAR <- 0
    pmMAR <- 0
 
    paramSet <- simParams[[1]]
    set.seed(seed)

    #1. Create Data. Misspec? Nonormal?
    if (class(obj) == "list") {
        data <- createData(paramSet, n, objData) # data.mis == simDataOut
        #if (!isNullObject(objData@indLab)) 
        #    colnames(data.mis@data) <- objData@indLab
    } else {
        data.mis <- obj # not sure what this signifies
    }

    #1.2 Impose Missing, arbitrary function?

    #2. Deal with labels
    Output <- NULL
    DataOut <- NULL
    #if (class(data) == "SimDataOut") {
    #    DataOut <- data
    #    data <- DataOut@data
    #}
    if (is.null(colnames(data))) 
        colnames(data) <- paste("y", 1:ncol(data))
    if (isNullObject(model@auxiliary)) {
        if (!isNullObject(simMissing) && !(length(simMissing@cov) == 1 && simMissing@cov == 0) && simMissing@covAsAux) 
            model@auxiliary <- simMissing@cov
    }
    # some quick fixes
    model <- objModel
    simMissing <- objMissing
    
    if (isNullObject(model@indLab)) {
        if (isNullObject(model@auxiliary)) {
            model@indLab <- colnames(data)
        } else if (is.numeric(model@auxiliary)) {
            if (max(model@auxiliary) > ncol(data)) 
                stop("The maximum index in the auxiliary variable set is greater than the number of variables in the data.")
            model@indLab <- colnames(data)[-model@auxiliary]
        } else {
            if (length(intersect(colnames(data), model@auxiliary)) != length(model@auxiliary)) 
                stop("Some auxiliary variables does not exist in the dataset.")
            model@indLab <- setdiff(colnames(data), model@auxiliary)
        }
    }
    if (is.numeric(model@indLab)) 
        model@indLab <- colnames(data)[model@indLab]
    if (is.numeric(model@auxiliary)) 
        model@auxiliary <- colnames(data)[model@auxiliary]
    if (length(intersect(model@auxiliary, model@indLab)) != 0) 
        stop("There is common variable between the variables in the model and the auxiliary variables.")

    targetCol <- c(model@indLab, model@auxiliary)
    data <- data[, targetCol]
    miss <- sum(is.na(data)) > 0
    
    #estimator <- tolower(estimator) - shouldn't be here, should be in simModel as a check already.
    
    #2. Deal With Constraints
     if (!isNullObject(model@equalCon)) {
        equalCon <- model@equalCon
        equalCon <- reduceConstraint(equalCon)
        con.text <- writeLavaanConstraint(param, equalCon)
    } else {
        con.text <- blankParameters(param)
    }

    #3. Run Lavaan
    fit <- NULL
    #if (modelType == "Path.exo" | modelType == "Path") {
    #    try(fit <- sem(code, data = Data, meanstructure = TRUE, missing = miss, fixed.x = FALSE, estimator = estimator))
    if(flag==FALSE){
        code <- writeLavaanCode2(param, con.text, aux = nameAux)
        try(fit <- sem(code, data = data, meanstructure = TRUE, missing = "listwise", estimator = model@estimator))
      } else {
         try(fit <- sem(pt, data = data, meanstructure = TRUE, missing = "listwise", estimator = model@estimator)) # currently, just grabs pt from the global scope
       }

    #4. Extract Fit
    FitIndices <- NA
    FitIndicesNull <- NA
    Converged <- FALSE
    if (!is.null(fit)) {
        try(FitIndices <- extractLavaanFit(fit))
        try(coef <- combineObject(param, inspect(fit, "coef")))
        try(se <- combineObject(param, inspect(fit, "se")))
        try(Converged <- inspect(fit, "converged"))
        try(check <- sum(unlist(lapply(inspect(fit, "se"), sum))))
        try(if (is.na(check) || check == 0) 
            Converged = FALSE, silent = TRUE)
    }
    #temp <- NULL
    # Impute missing and run results
    #try(temp <- runModel(model = objModel, data = data.mis, flag=flag))
      
    if (Converged) {
        converged <- temp@converged
        param <- NA
        Labels <- makeLabels(temp@param, "OpenMx")  #As a quick default to use OpenMx
        if (converged) {
            coef <- vectorizeObject(temp@coef, Labels)
            se <- vectorizeObject(temp@se, Labels)
            fit <- temp@fit
            stdSet <- standardize(temp)
            std <- vectorizeObject(stdSet, Labels)
           
            if (!isNullObject(temp@paramValue)) {
                param <- vectorizeObject(temp@paramValue, Labels)
            } else {
                param <- NA
            }
        }
    } else {
        if (!is.null(data.mis) && is(data.mis, "SimDataOut")) #not sure what this means, but I'm sure I've introduced bugs.
            param <- NA
    }

        if (!is.null(DataOut)) {
        param <- DataOut@param
        check <- all.equal(param, Output@param)
        usedX <- NULL
        usedY <- NULL
        if (!(length(check) == 1 && check == TRUE) & !isNullObject(model@auxiliary)) {
            usedY <- which(!(colnames(data) %in% model@auxiliary))
            nx <- 0
            if (model@modelType == "SEM.exo") 
                nx <- nrow(model@param@LX)
            if (model@modelType == "Path.exo") 
                nx <- nrow(model@param@PH)
            if (nx > 0) 
                usedX <- intersect(1:nx, usedY)
            usedY <- setdiff(usedY, usedX)
            param <- extract(param, y = usedY, x = usedX)
        }
        check <- all.equal(param, Output@param)
        if (length(check) == 1 && check == TRUE) {
            paramOut <- DataOut@paramOut
            if (!isNullObject(model@auxiliary)) 
                paramOut <- extract(paramOut, y = usedY, x = usedX)
            Output@paramValue <- paramOut
        }
    }
    Output@n <- nrow(data)
    if (!isNullObject(model@indLab)) {
        Output@indLab <- model@indLab
    } else {
        Output@indLab <- colnames(data)
    }
    Output@factorLab <- model@factorLab
    # Add labels in the SimModelOut --> go to SimModelOut and relabels it
    
    # Provide a nicer summary --> Groups elements from the same matrix together
    
    LabelsDataParam <- makeLabels(createFreeParameters(objData@param), "OpenMx")
    paramData <- vectorizeObject(simParams[[1]]$real, LabelsDataParam)
    Result <- list(coef = coef, se = se, fit = fit, converged = converged, param = param, FMI1 = FMI1, FMI2 = FMI2, std = std, paramData = paramData)
    return <- Result
}

## runmodel <- function(model, data, simMissing = new("NullSimMissing"), estimator = NULL,flag=flag) { 
  

##    Output <- runLavaan2(model, data, miss = "listwise", estimator = estimator,flag=flag)
    
##    # is.equal(DataOut@param, Output@param) yes --> compute bias

##  }

## runLavaan2 <- function(model, Data, miss = "fiml", estimator = "ML",flag=flag) {
  
##     Data <- as.data.frame(Data)
##     ni <- ncol(Data)
##     param <- model@param
##     modelType <- model@modelType
##     varnames <- NULL
##     nz <- 0
   
    
##    for (i in 1:(ncol(Data) - nz)) {
##             temp <- paste("y", i, sep = "")
##             varnames <- c(varnames, temp)
##    }
    
##     nameAux <- NULL
   
##     colnames(Data) <- varnames
##     param <- tagHeaders(param)
##     con.text <- NULL
   
##     coef <- new("SimRSet")
##     se <- new("SimRSet")
##     name <- slotNames(param)
##     for (i in 1:length(name)) {
##         slot(coef, name[i]) <- slot(param, name[i])
##         slot(se, name[i]) <- slot(param, name[i])
##     }
 
##     return(new("SimModelOut", param = model@param, start = model@start, equalCon = model@equalCon, package = model@package,
##                coef = coef, fit = FitIndices, se = se, converged = Converged))
## }

writeLavaanCode2 <- function(object, constraint, aux = NULL) {
    result <- NULL
    object <- collapseExo(object, label = TRUE)
    constraint <- collapseExo(constraint, label = TRUE, value = NA)  ###################Have some zeros
    if (!isNullObject(object@LY)) {
        for (i in 1:ncol(object@LY)) {
            temp <- paste(colnames(object@LY)[i], "=~")
            something <- FALSE
            for (j in 1:nrow(object@LY)) {
                if (is.na(object@LY[j, i]) | object@LY[j, i] != 0 | !is.na(constraint@LY[j, i])) {
                  content <- paste(object@LY[j, i], "*", sep = "")
                  if (!is.na(constraint@LY[j, i])) 
                    content <- constraint@LY[j, i]
                  temp <- paste(temp, " ", content, rownames(object@LY)[j], sep = "")
                  something <- TRUE
                }
                if (something && j != nrow(object@LY) && (is.na(object@LY[j + 1, i]) | object@LY[j + 1, i] != 0)) 
                  temp <- paste(temp, "+")
            }
            if ((sum(is.na(object@LY[, i])) == 0) && (sum(object@LY[, i]) == 0)) 
                temp <- paste(temp, "0*", rownames(object@LY)[1], sep = "")
            if (!is.null(aux)) {
                noVar <- !(is.na(diag(object@TE)) | diag(object@TE) != 0)
                lab <- "0"
                if (sum(noVar) > 0) {
                  LYsingle <- extract(object@LY, which(noVar), i)
                  LYsingle <- is.na(LYsingle) | LYsingle != 0
                  if (sum(LYsingle) > 0) 
                    lab <- "NA"
                }
                temp <- paste(temp, paste(paste(" + ", lab, "*", aux, sep = ""), collapse = ""))
            }
            result <- paste(result, temp, "\n")
        }
    }
    if (!isNullObject(object@BE)) {
        for (i in 1:nrow(object@BE)) {
            temp <- NULL
            for (j in 1:ncol(object@BE)) {
                if (is.na(object@BE[i, j]) | object@BE[i, j] != 0 | !is.na(constraint@BE[i, j])) {
                  content <- paste(object@BE[i, j], "*", sep = "")
                  if (!is.na(constraint@BE[i, j])) 
                    content <- constraint@BE[i, j]
                  temp <- paste(temp, " ", content, colnames(object@BE)[j], sep = "")
                }
                if (!is.null(temp) && j != ncol(object@BE) && (is.na(object@BE[i, j + 1]) | object@BE[i, j + 1] != 0)) 
                  temp <- paste(temp, "+")
            }
            if (!is.null(temp)) {
                temp2 <- paste(rownames(object@BE)[i], "~")
                result <- paste(result, temp2, temp, "\n")
            }
        }
        if (isNullObject(object@LY) && !is.null(aux)) {
            set <- findRecursiveSet(object@BE)
            target <- colnames(object@BE)[set[[1]]]
            for (i in 1:length(aux)) {
                temp <- paste(aux[1], " ~ ", paste(paste("NA*", target), collapse = " + "), "\n", sep = "")
                result <- paste(result, temp)
            }
        }
    }
    if (!isNullObject(object@PS)) {
        var.code <- NULL
        for (i in 1:length(diag(object@PS))) {
            if (!is.na(object@PS[i, i]) | !is.na(constraint@PS[i, i])) {
                content <- paste(object@PS[i, i], "*", sep = "")
                if (!is.na(constraint@PS[i, i])) 
                  content <- constraint@PS[i, i]
                var.code <- paste(var.code, colnames(object@PS)[i], " ~~ ", content, colnames(object@PS)[i], " \n", sep = "")
            }
        }
        cov.code <- NULL
        if (nrow(object@PS) > 1) {
            for (i in 2:nrow(object@PS)) {
                for (j in 1:(i - 1)) {
                  if (is.na(object@PS[i, j]) | object@PS[i, j] != 0 | !is.na(constraint@PS[i, j])) {
                    content <- paste(object@PS[i, j], "*", sep = "")
                    if (!is.na(constraint@PS[i, j])) 
                      content <- constraint@PS[i, j]
                    if (isNullObject(object@BE)) {
                      cov.code <- paste(cov.code, rownames(object@PS)[i], " ~~ ", content, colnames(object@PS)[j], " \n", sep = "")
                    } else {
                      exo.set <- findRecursiveSet(object@BE)[[1]]
                      if (!(is.element(i, exo.set) & is.element(j, exo.set))) {
                        cov.code <- paste(cov.code, rownames(object@PS)[i], " ~~ ", content, colnames(object@PS)[j], " \n", sep = "")
                      }
                    }
                  } else {
                    content <- paste(object@PS[i, j], "*", sep = "")
                    if (isNullObject(object@BE)) {
                      cov.code <- paste(cov.code, rownames(object@PS)[i], " ~~ ", content, colnames(object@PS)[j], " \n", sep = "")
                    } else {
                      auxFac <- which(apply(object@BE, 1, function(x) all(!is.na(x) & (x == 0))) & apply(object@BE, 2, function(x) all(!is.na(x) & (x == 0))))
                      if (is.element(i, auxFac) | is.element(j, auxFac)) {
                        cov.code <- paste(cov.code, rownames(object@PS)[i], " ~~ 0*", colnames(object@PS)[j], " \n", sep = "")
                      }
                    }
                  }
                }
            }
        }
        result <- paste(result, var.code, cov.code)
        if (isNullObject(object@LY) && !is.null(aux)) {
            set <- findRecursiveSet(object@BE)
            target <- colnames(object@BE)[-set[[1]]]
            varCode <- paste(paste(aux, " ~~ NA*", aux, sep = ""), collapse = "\n")
            result <- paste(result, varCode, "\n")
            corCode <- paste(outer(aux, target, paste, sep = " ~~ NA*"), collapse = "\n")
            result <- paste(result, corCode, "\n")
        }
    }
    if (!isNullObject(object@TE)) {
        var.code <- NULL
        for (i in 1:length(diag(object@TE))) {
            if (!is.na(object@TE[i, i]) | !is.na(constraint@TE[i, i])) {
                content <- paste(object@TE[i, i], "*", sep = "")
                if (!is.na(constraint@TE[i, i])) 
                  content <- constraint@TE[i, i]
                var.code <- paste(var.code, colnames(object@TE)[i], " ~~ ", content, colnames(object@TE)[i], " \n", sep = "")
            }
        }
        cov.code <- NULL
        for (i in 2:nrow(object@TE)) {
            for (j in 1:(i - 1)) {
                if (is.na(object@TE[i, j]) | object@TE[i, j] != 0 | !is.na(constraint@TE[i, j])) {
                  content <- paste(object@TE[i, j], "*", sep = "")
                  if (!is.na(constraint@TE[i, j])) 
                    content <- constraint@TE[i, j]
                  cov.code <- paste(cov.code, rownames(object@TE)[i], " ~~ ", content, colnames(object@TE)[j], " \n", sep = "")
                }
            }
        }
        result <- paste(result, var.code, cov.code)
        if (!isNullObject(object@LY) && !is.null(aux)) {
            nonConstant <- is.na(diag(object@TE)) | diag(object@TE) != 0
            target <- colnames(object@TE)[nonConstant]
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
    if (!isNullObject(object@AL)) {
        mean.code <- NULL
        for (i in 1:length(object@AL)) {
            if (!(object@modelType == "Path" | object@modelType == "Path.exo") | !is.na(object@AL[i]) | !is.na(constraint@AL[i])) {
                content <- paste(object@AL[i], "*", sep = "")
                if (!is.na(constraint@AL[i])) 
                  content <- constraint@AL[i]
                mean.code <- paste(mean.code, names(object@AL)[i], " ~ ", content, "1 \n", sep = "")
            }
        }
        result <- paste(result, mean.code)
    }
    if (!isNullObject(object@TY)) {
        mean.code <- NULL
        for (i in 1:length(object@TY)) {
            content <- paste(object@TY[i], "*", sep = "")
            if (!is.na(constraint@TY[i])) 
                content <- constraint@TY[i]
            mean.code <- paste(mean.code, names(object@TY)[i], " ~ ", content, "1 \n", sep = "")
        }
        result <- paste(result, mean.code)
    }
    if (!is.null(aux)) {
        temp <- paste(paste(aux, " ~ NA*1 \n"), collapse = "")
        result <- paste(result, temp)
    }
    return(result)
} 


combineReps <- function(Result.l,nRep,seed,n,pmMAR,pmMCAR) {
  fit.l <- lapply(Result.l, function(object) {
        object$fit
    })
    coef.l <- lapply(Result.l, function(object) {
        object$coef
    })
    se.l <- lapply(Result.l, function(object) {
        object$se
    })
    converged.l <- lapply(Result.l, function(object) {
        object$converged
    })
    param.l <- lapply(Result.l, function(object) {
        object$param
    })
    FMI1.l <- lapply(Result.l, function(object) {
        object$FMI1
    })
    FMI2.l <- lapply(Result.l, function(object) {
        object$FMI2
    })
    std.l <- lapply(Result.l, function(object) {
        object$std
    })
    paramData.l <- lapply(Result.l, function(object) {
        object$paramData
    })
    coef <- as.data.frame(do.call(rbind, coef.l))
    se <- as.data.frame(do.call(rbind, se.l))
    fit <- as.data.frame(do.call(rbind, fit.l))
    FMI1 <- as.data.frame(do.call(rbind, FMI1.l))
    FMI2 <- as.data.frame(do.call(rbind, FMI2.l))
    std <- as.data.frame(do.call(rbind, std.l))
    paramData <- as.data.frame(do.call(rbind, paramData.l))
    converged <- as.vector(unlist(converged.l))
    param <- new("NullDataFrame")
    FMI1 <- new("NullDataFrame")
    FMI2 <- new("NullDataFrame")
    if (!is.null(param.l)) {
        param <- as.data.frame(do.call(rbind, param.l))
        if (sum(dim(param)) == 0) 
            param <- new("NullDataFrame")
        if (nrow(unique(param)) == 1) 
            param <- unique(param)
    }
    if (!is.null(FMI1.l)) {
        FMI1 <- as.data.frame(do.call(rbind, FMI1.l))
        if (sum(dim(FMI1)) == 0) 
            FMI1 <- new("NullDataFrame")
        if (nrow(unique(FMI1)) == 1) 
            FMI1 <- unique(FMI1)
    }
    if (!is.null(FMI2.l)) {
        FMI2 <- as.data.frame(do.call(rbind, FMI2.l))
        if (sum(dim(FMI2)) == 0) 
            FMI2 <- new("NullDataFrame")
        if (nrow(unique(FMI2)) == 1) 
            FMI2 <- unique(FMI2)
    }
    if (is.null(n)) {
        if (class(objData) == "SimData") {
            if (is.null(n)) 
                n <- objData@n
        } else if (is.list(objData)) {
            if (class(objData[[1]]) == "SimDataOut") {
                n <- objData@n
            } else if (is.matrix(objData[[1]]) | is.data.frame(objData[[1]])) {
                n <- nrow(objData[[1]])
            }
        }
    }
    if (is.null(pmMCAR)) 
        ifelse(isNullObject(objMissing), pmMCAR <- 0, pmMCAR <- objMissing@pmMCAR)
    if (is.null(pmMAR)) 
        ifelse(isNullObject(objMissing), pmMAR <- 0, pmMAR <- objMissing@pmMAR)
    if (nrow(param) == 1 & ncol(param) == 1 && is.na(param)) 
        param <- paramData

    Result <- new("SimResult", modelType = modelType, nRep = nRep, coef = coef, se = se, fit = fit, converged = converged,
                  seed = seed, paramValue = param, FMI1 = FMI1, FMI2 = FMI2, stdCoef = std, n = n, pmMCAR = pmMCAR, pmMAR = pmMAR)
  return(Result)
}
