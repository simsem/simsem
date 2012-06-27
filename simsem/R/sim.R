
  sim <- function(nRep, model, n, generate = NULL, rawData = NULL,
                  miss = NULL, fun=NULL,
                  pmMCAR = NULL, pmMAR = NULL,
                  facDist = NULL, indDist = NULL, errorDist = NULL, sequential = FALSE, 
                  modelBoot = FALSE, realData = NULL, maxDraw = 50, misfitType = "f0", misfitBounds = NULL, averageNumMisspec = NULL, optMisfit=NULL, optDraws = 50, 
                  aux = NULL, 
                  seed = 123321, silent = FALSE, multicore = FALSE, cluster = FALSE, numProc = NULL,  
                  paramOnly = FALSE, dataOnly=FALSE, ...) {

    ## 1. Set up correct data generation template
    dgen <- NULL
    if(!is.null(generate) && class(generate) == "SimSem") {
      model@dgen <- generate@dgen
    }

    ## 2. Compute combinations of simulation parameters (MAR, MCAR, n): complete factorial
    set.seed(seed)
    warnT <- as.numeric(options("warn"))
    if (silent) 
        options(warn = -1)
    if (is.null(nRep)) {
        if (!is.vector(n)) 
            stop("Please specify the number of replications")
        if (!is.null(pmMCAR) && !is.vector(pmMCAR)) 
            stop("Please specify the number of replications")
        if (!is.null(pmMAR) && !is.vector(pmMAR)) 
            stop("Please specify the number of replications")
        usedMCAR <- NULL
        usedMAR <- NULL
        ifelse(is.null(pmMCAR), usedMCAR <- 1, usedMCAR <- pmMCAR)
        ifelse(is.null(pmMAR), usedMAR <- 1, usedMAR <- pmMAR)
        out <- expand.grid(n, usedMCAR, usedMAR)
        n <- out[, 1]
        if (!is.null(pmMCAR)) 
            pmMCAR <- out[, 2]
        if (!is.null(pmMAR)) 
            pmMAR <- out[, 3]
        nRep <- nrow(out)
    } else if(is.null(rawData)) { # If there is no raw data, compute correct n for each condition
      if(length(n) != nRep) {
        if(length(n) == 1) {
          n <- rep(n,nRep)
        } else if (length(n) < nRep && (nRep %% length(n)) == 0) {
          n <- rep(n,each=(nRep/length(n)))
        } else {
          stop("n can only be of length 1, of length nRep, or a multiple of nRep")
        }
      }
    }

    ## 3. Draws for randomly varying simulation parameters
    if (!is.null(n)) {
      if (is(n, "VirtualDist")) {
        n <- round(run(n, nRep))
      } else if (is.vector(n)) {
        if (length(n) != nRep) 
          ifelse(length(n) > nRep, n <- sample(n, nRep, replace = TRUE), n <- sample(n, nRep))
      } else {
        stop("The n argument should be in a vector of numbers or distribution object only.")
      }
    }
    if (!is.null(pmMCAR)) {
      if (is(pmMCAR, "VirtualDist")) {
        pmMCAR <- run(pmMCAR, nRep)
      } else if (is.vector(pmMCAR)) {
        if (length(pmMCAR) != nRep) 
          ifelse(length(pmMCAR) > nRep, pmMCAR <- sample(pmMCAR, nRep, replace = TRUE), pmMCAR <- sample(pmMCAR, nRep))
      } else {
        stop("The pmMCAR argument should be in a vector of numbers or distribution object only.")
      }
    }
    if (!is.null(pmMAR)) {
      if (is(pmMAR, "VirtualDist")) {
        pmMAR <- run(pmMAR, nRep)
      } else if (is.vector(pmMAR)) {
        if (length(pmMAR) != nRep) 
          ifelse(length(pmMAR) > nRep, pmMAR <- sample(pmMAR, nRep, replace = TRUE), pmMAR <- sample(pmMAR, nRep))
      } else {
        stop("The pmMAR argument should be in a vector of numbers or distribution object only.")
      }
    }
   
    ## 4. Build list of simulation conditions. Each element of simConds is a replication.
    ##modelType <- model@modelType
    param <- NULL
    drawnParams <- list()
    simConds <- list()

    numseed <- as.list(round(sample(1:999999, nRep)))
    if (is.null(rawData)) {
      ##       if (any(sapply(dgen,fun=function(paramSet) { sapply(paramSet, fun=function(mat) {is.random(mat@popParam)}) }))) {
      ##         for (i in 1:nRep) {
      ##           drawnParams[[i]] <- drawParam(model@dgen, maxDraw = maxDraw, numFree=max(model@pt$free), misfitBounds=misfitBounds, averageNumMisspec=averageNumMisspec,
      ##                                         optMisfit=optMisfit, optDraws=optDraws, misfitType=misfitType)
      ##         }
      ##       } else {
      ##         fixedParam <- drawParam(model@dgen, maxDraw = maxDraw, numFree=max(model@pt$free), misfitBounds=misfitBounds, averageNumMisspec=averageNumMisspec,
      ##                                 optMisfit=optMisfit, optDraws=optDraws, misfitType=misfitType)
      ##         for (i in 1:nRep) {
      ##           drawnParams[[i]] <- fixedParam
      ##         }
      ##       }
      
      for (i in seq_len(nRep)) {
        simConds[[i]] <- list()
        simConds[[i]][[1]] <- NULL
        simConds[[i]][[2]] <- n[i]
        simConds[[i]][[3]] <- pmMCAR[i]
        simConds[[i]][[4]] <- pmMAR[i]
        simConds[[i]][[5]] <- numseed[[i]]       
      }
      
    } else if (is.list(rawData)) {

      if (is.data.frame(rawData[[1]])) {            
        if (!is.null(n) && ((n > nrow(rawData[[1]])) %in% TRUE)) 
          stop("The specified n is greater than the number of cases provided.")
      } else if (is.matrix(rawData[[1]])) {
        rawData <- lapply(rawData, data.frame)
        n <- lapply(rawData,nrow) # Find n for rawData
        if (!is.null(n) && ((n > nrow(rawData[[1]])) %in% TRUE)) 
          stop("The specified n is greater than the number of cases provided.")
        ## } else if (class(rawData[[1]]) == "SimDataOut") {         
        ##         if (!is.null(n) && ((n > rawData@n) %in% TRUE)) 
        ##           stop("The specified n is greater than the number of cases provided.")
      } else {
        stop("The list in the rawData argument does not contain matrices or data frames.")
      }
      
      for (i in seq_along(rawData)) {
        simConds[[i]] <- list()
        simConds[[i]][[1]] <- rawData[[i]]
        simConds[[i]][[2]] <- n[i]
        simConds[[i]][[3]] <- pmMCAR[i]
        simConds[[i]][[4]] <- pmMAR[i]
        simConds[[i]][[5]] <- numseed[[i]]
      }
    } else {
      stop("The rawData argument is not a SimData class or a list of data frames.")
    }
    
    
    ## 5. Run replications
    if (multicore) {
      library(parallel)
      sys <- .Platform$OS.type
      if (is.null(numProc)) 
        numProc <- detectCores()
      if (sys == "windows") {
        cl <- makeCluster(rep("localhost", numProc), type = "SOCK")                        
        Result.l <- clusterApplyLB(cl, simConds, runRep, miss = miss, fun = fun, silent = silent,
                                   facDist = facDist, indDist = indDist, errorDist=errorDist, sequential=sequential, realData=realData,
                                   maxDraw = maxDraw, misfitBounds=misfitBounds, averageNumMisspec=averageNumMisspec,
                                   optMisfit=optMisfit, optDraws=optDraws, misfitType=misfitType)
        stopCluster(cl)
      } else {
        Result.l <- mclapply(simConds, runRep, model, miss = miss, fun = fun,  silent = silent,
                             facDist = facDist, indDist = indDist, errorDist=errorDist, sequential=sequential, realData=realData,
                             maxDraw = maxDraw, misfitBounds=misfitBounds, averageNumMisspec=averageNumMisspec,
                             optMisfit=optMisfit, optDraws=optDraws, misfitType=misfitType, mc.cores = numProc)
      }
    } else {
      Result.l <- lapply(simConds, runRep, model, miss = miss, fun = fun, silent = silent,
                         facDist = facDist, indDist = indDist, errorDist=errorDist, sequential=sequential, realData=realData,
                         maxDraw = maxDraw, misfitBounds=misfitBounds, averageNumMisspec=averageNumMisspec,
                         optMisfit=optMisfit, optDraws=optDraws, misfitType=misfitType)
    }

    ## 6. Extract results from replication lists
      
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
   ##  paramData.l <- lapply(Result.l, function(object) {
##         object$paramData
##     })
    coef <- as.data.frame(do.call(rbind, coef.l))
    se <- as.data.frame(do.call(rbind, se.l))
    fit <- as.data.frame(do.call(rbind, fit.l))
    std <- as.data.frame(do.call(rbind, std.l))
    #paramData <- as.data.frame(do.call(rbind, paramData.l))
    converged <- as.vector(unlist(converged.l))
    param <- NULL
    FMI1 <- NULL
    FMI2 <- NULL
    if (!is.null(param.l[[1]])) {
        param <- as.data.frame(do.call(rbind, param.l))
        if (sum(dim(param)) == 0) 
            param <- NULL
        if (nrow(unique(param)) == 1) 
            param <- unique(param)
    }
    if (!is.null(FMI1.l[[1]])) {
        FMI1 <- as.data.frame(do.call(rbind, FMI1.l))
        if (sum(dim(FMI1)) == 0) 
            FMI1 <- NULL
        if (nrow(unique(FMI1)) == 1) 
            FMI1 <- unique(FMI1)
    }
    if (!is.null(FMI2.l[[1]])) {
        FMI2 <- as.data.frame(do.call(rbind, FMI2.l))
        if (sum(dim(FMI2)) == 0) 
            FMI2 <- NULL
        if (nrow(unique(FMI2)) == 1) 
            FMI2 <- unique(FMI2)
    }
    if (is.null(n)) {
##         if (class(objData) == "SimData") {
##             if (is.null(n)) 
##                 n <- objData@n
##         } else if (is.list(objData)) {
##             if (class(objData[[1]]) == "SimDataOut") {
##                 n <- objData@n
           ##  } else
    if (is.matrix(objData[[1]]) | is.data.frame(objData[[1]])) {
                n <- nrow(objData[[1]])
              }
  }
  
    if (is.null(pmMCAR)) 
        ifelse(is.null(miss), pmMCAR <- 0, pmMCAR <- objMissing@pmMCAR)
    if (is.null(pmMAR)) 
        ifelse(is.null(miss), pmMAR <- 0, pmMAR <- objMissing@pmMAR)
    if (nrow(param) == 1 & ncol(param) == 1 && is.na(param)) 
        param <- paramData
    Result <- new("SimResult", modelType = model@modelType, nRep = nRep, coef = coef, se = se, fit = fit, converged = converged, seed = seed, paramValue = param, FMI1 = data.frame(FMI1), FMI2 = data.frame(FMI2), stdCoef = std, 
        n = n, pmMCAR = pmMCAR, pmMAR = pmMAR)
    if (silent) 
        options(warn = warnT)
    return <- Result
}

# runRep: Run one replication

runRep <- function(simConds, model, miss = NULL, fun=NULL, facDist = NULL, indDist = NULL, errorDist = NULL, sequential = FALSE, realData=NULL, silent = FALSE,
                   modelBoot = FALSE, maxDraw = 50, misfitType = "f0", misfitBounds = NULL, averageNumMisspec = NULL, optMisfit=NULL, optDraws = 50) {
    param <- NULL
    coef <- NA
    se <- NA
    fit <- NA
    std <- NA
    FMI1 <- NULL
    FMI2 <- NULL
    converged <- FALSE
    seed <- simConds[[5]]
    n <- simConds[[2]]
    pmMCAR <- simConds[[3]]
    pmMAR <- simConds[[4]]
    dgen <- model@dgen

    ## 1. Create a missing data template from simulation parameters.
    if (is.null(miss)) {
        if (!is.null(pmMAR) | !is.null(pmMCAR)) {
            if (is.null(pmMCAR)) 
                pmMCAR <- 0
            if (is.null(pmMAR)) 
                pmMAR <- 0
            miss <- simMissing(pmMCAR = pmMCAR, pmMAR = pmMAR)
        }
      }

    ## 2. Generate data (data) & store parameter values (paramSet)
    data <- simConds[[1]] # either a paramSet or raw data
    set.seed(seed)
    if (is.null(data)) { # Need to draw parameters
      ##data <- createData(paramSet = dat, indDist = indDist, sequential=sequential, facDist = facDist, errorDist=errorDist,
      ##                   indLab = indLab, modelBoot=modelBoot, realData=realData)
      ##  datal <- mapply(FUN=createData,draws,indDist,facDist,errorDist,
      ##                   MoreArgs=list(n=n, sequential=sequential, modelBoot=modelBoot,realData=realData), SIMPLIFY=FALSE)
      ##         data <- do.call("rbind",datal)
      ##         data <- cbind(data,group=rep(1:ngroups,each=n))
      genout <- generate(model=model, n=n, maxDraw=maxDraw, misfitBounds=misfitBounds, misfitType=misfitType, averageNumMisspec=averageNumMisspec,
                       optMisfit=optMisfit, optDraws=optDraws, indDist=indDist, sequential=sequential, facDist=facDist, errorDist=errorDist,
                       indLab=indLab, modelBoot=modelBoot, realData=realData, params=TRUE)
      data <- genout[[1]]
      psl <- genout[[2]] # Indexing: Group -> param/misParam/misOnly -> paramSet (reduced)
      if(!is.null(psl[[1]]$misParam)) {
        paramSet <- lapply(psl,"[[",2) # Group -> misParam -> paramSet (by group)
      } else {
        paramSet <- lapply(psl,"[[",1) # Group -> param -> paramSet (by group)
      }
    } # else: do nothing. Raw data.

    ## 3. Impose Missing (if any)
    if (!is.null(miss)) {
        
      data <- imposeMissing(data, cov=miss@cov, pmMCAR=pmMCAR, pmMAR=pmMAR, nforms=miss@nforms, itemGroups=miss@itemGroups,
                            twoMethod=miss@twoMethod, prAttr=miss@prAttr, timePoints=miss@timePoints, logical=miss@logical, ignoreCols=miss@ignoreCols,
                            threshold=miss@threshold)
    }

    ## 4. Call user function (if exists)
    if (!is.null(fun)) {
      data <- fun(data) # args??
      #data.mis <- run(objFunction, data.mis, checkDataOut = TRUE)
    }

    ## 5. Call lavaan using simsem template and generated data from 2.
    out <- NULL
    # Impute missing and run results
    if (!is.null(miss) && miss@numImps > 0) {
      if (silent) {
        invisible(capture.output(suppressMessages(try(out <- analyze(model, data, simMissing=miss),silent=TRUE))))
        ##invisible(capture.output(suppressMessages(try(temp <- run(simConds = objModel, data = data.mis, simMissing = miss), silent = TRUE))))
        ##invisible(capture.output(suppressMessages(try(temp <- runMI(data.mis,objModel,miss@numImps,miss@impMethod), silent=TRUE))))
      } else {
        try(out <- analyze(model, data, simMissing=miss))
        ##try(temp <- run(simConds = objModel, data = data.mis, simMissing = miss), silent = TRUE)
        ## try(temp <- runMI(data.mis,objModel,miss@numImps,miss@impMethod))
      }
    } else {
      if (silent) {
        invisible(capture.output(suppressMessages(try(out <- anal(model,data),silent=TRUE))))
        ## tryCatch(temp <- run(objModel, data), error=function(e) {print('Error')})
      } else {
        try(out <- anal(model,data))
      }
    }
  
    ## 6. Parse Lavaan Output
    if (!is.null(out)) {
      try(se <- inspect(out,"se"))
      try(converged <- inspect(out, "converged"))
      try(check <- sum(unlist(lapply(se, sum))))
      try(if(is.na(check) || check==0) {converged <- FALSE},silent=TRUE)
    }
    
    if (converged) {
      fit <- extractLavaanFit(out)
      coef <- reduceLavaanParam(inspect(out,"coef"),dgen)
      se <- reduceLavaanParam(se,dgen)
      std <- reduceLavaanParam(standardize(out), dgen)
      if(!is.null(paramSet)) {
        popParam <- reduceParamSet(paramSet,dgen)
      } else {
        popParam <- NA # Real Data
      }
    } else {
      popParam <- NA # Fail to converge
    }
    ##  if (is(temp, "SimModelMIOut")) {
    
    ##           FMI1 <- vectorizeObject(temp@FMI1, Labels)
    ##           FMI2 <- vectorizeObject(temp@FMI2, Labels)
    ##         }
    
    ##   ##} else {
    ##       if (!is.null(data.mis) && is(data.mis, "SimDataOut")) 
    ##         param <- NA
    
    Result <- list(coef = coef, se = se, fit = fit, converged = converged, param = popParam, FMI1 = FMI1, FMI2 = FMI2, std = std)
    Result
}

extractLavaanFit <- function(Output) {
    Indices <- fitmeasures(Output)
    result <- c(Indices["chisq"], Indices["df"], Indices["pvalue"], Indices["baseline.chisq"], Indices["baseline.df"], Indices["baseline.pvalue"],
                Indices["cfi"], Indices["tli"], Indices["aic"], Indices["bic"], Indices["rmsea"], Indices["rmsea.ci.lower"], Indices["rmsea.ci.upper"], Indices["srmr"])
    old.name <- c("chisq", "cfi", "tli", "aic", "bic", "rmsea", "srmr")
    new.name <- c("Chi", "CFI", "TLI", "AIC", "BIC", "RMSEA", "SRMR")
    name <- names(result)
    for (i in 1:length(old.name)) {
        name <- gsub(old.name[i], new.name[i], name)
    }
    names(result) <- name
    result
}

## paramSet -> Re-labeled population parameter values (for free parameters)
reduceParamSet <- function(paramSet,dgen) {
  if(!is.list(dgen[[1]])) { dgen <- list(dgen) }
  final <- NULL
  for(g in seq_along(paramSet)) {
    tpset <- paramSet[[g]]
    for(i in seq_along(tpset)) {
      if(!is.null(tpset[[i]])) {
        tmat <- tpset[[i]]
        name <- names(tpset)[[i]]
        idx <- which(names(dgen[[g]])==name)
        if(is.null(dgen[[g]][[idx]])) {
          idx <- which(names(dgen[[g]])==paste0("R",name))
        }
        if(length(idx) != 0) {
          free <- is.free(dgen[[g]][[idx]]@free)
          param <- tmat[free]
          if(name == "PS" || name == "TE" ) {
            lab <- makeLabels(free,name=name,symmetric=TRUE)
          } else {
            lab <- makeLabels(free,name=name)
          }
          names(param) <- lab[!is.na(lab)]
          final <- c(final,param)
        }
      }
    }
  }
  final
}    
      
    

## GLIST -> Re-labeled Parameter Estimates
reduceLavaanParam <- function(glist,dgen) {
  # Chunk at a time approach
  names <- names(glist)
  final <- NULL
  if(!is.list(dgen[[1]])) { dgen <- list(dgen) }

  if("lambda" %in% names) {
    idx <- which(names=="lambda")
    for(i in seq_along(idx)) {
      free <- is.free(dgen[[i]]$LY@free)
      param <- glist[idx[i]]$lambda[free]
      lab <- makeLabels(free,name="LY")
      names(param) <- lab[!is.na(lab)]
      final <- c(final,param)
    }
  }
  
  if("theta" %in% names) {  
    idx <- which(names=="theta")
    for(i in seq_along(idx)) {
      if(!is.null(dgen[[i]]$TE)) {
        free <- is.free(dgen[[i]]$TE@free)
      } else {
        free <- is.free(dgen[[i]]$RTE@free)
      }
      param <- glist[idx[i]]$theta[free]
      lab <- makeLabels(free,name="TE",symmetric=TRUE)
      names(param) <- lab[!is.na(lab)]
      final <- c(final,param)
    }
  }
  
  if("psi" %in% names) {  
    idx <- which(names=="psi")
    for(i in seq_along(idx)) {
      if(!is.null(dgen[[i]]$PS)) {
        free <- is.free(dgen[[i]]$PS@free)
      } else {
        free <- is.free(dgen[[i]]$RPS@free)
      }
      param <- glist[idx[i]]$psi[free]
      lab <- makeLabels(free,name="PS",symmetric=TRUE)
      names(param) <- lab[!is.na(lab)]
      final <- c(final,param)
    }
  }
  
  if("beta" %in% names) {  
    idx <- which(names=="beta")
    for(i in seq_along(idx)) {
      free <- is.free(dgen[[i]]$BE@free)
      param <- glist[idx[i]]$beta[free]
      lab <- makeLabels(free,name="BE")
      names(param) <- lab[!is.na(lab)]
      final <- c(final,param)
    }
  }

  if("alpha" %in% names && !is.null(dgen[[i]]$AL)) {  
    idx <- which(names=="alpha")
    for(i in seq_along(idx)) {
      free <- is.free(dgen[[i]]$AL@free)
      param <- glist[idx[i]]$alpha[free]
      lab <- makeLabels(free,name="AL")
      names(param) <- lab[!is.na(lab)]
      final <- c(final,param)
    }
  }

   if("nu" %in% names) {  
    idx <- which(names=="nu")
    for(i in seq_along(idx)) {
      free <- is.free(dgen[[i]]$TY@free)
      param <- glist[idx[i]]$nu[free]
      lab <- makeLabels(free,name="TY")
      names(param) <- lab[!is.na(lab)]
      final <- c(final,param)
    }
  }

  return(final)
}

makeLabels <- function(dat, name, symmetric=FALSE) {

  if (is.null(dat)) {
    return(NULL)
  } else if(is.vector(dat)) {
    Length <- length(dat)
    for (i in 1:Length) {
      ifelse(dat[i], dat[i] <- paste(name, i, sep = ""), dat[i] <- NA)
    }
    return(dat)
    
  } else if(is.matrix(dat)) {
    np <- nrow(dat)
    nq <- ncol(dat)
    
    if (symmetric) {
      for (i in 1:np) {
        for (j in 1:i) {
          if (dat[i, j]) {
            dat[i, j] <- paste(name, i, "_", j, sep = "")
          } else {
            dat[i, j] <- NA
          }
          if (i != j) 
            dat[j, i] <- dat[i, j]
        }
      }
    } else {
      for (i in 1:np) {
        for (j in 1:nq) {
          if (dat[i, j]) {
            dat[i, j] <- paste(name, i, "_", j, sep = "")
          } else {
            dat[i, j] <- NA
          }
        }
      }
    }
  }
  return(dat)
}

is.random <- function(dat) {
  dat[is.empty(dat)] <- "0" # Since we are trying to detect characters, we need to assign an arbitrary numeric value to ""
  isRandom <- sapply(dat, FUN=function(x) {x <- suppressWarnings(is.na(as.numeric(x))) })
  return(isRandom)
}

## Lavaan -> GLIST (std)
## taken shamelessly from param.value in lavaan.
standardize <- function(object) {

  GLIST <- object@Model@GLIST
  est.std <- standardizedSolution(object)$est.std

  for(mm in 1:length(GLIST)) {
    ## labels
    dimnames(GLIST[[mm]]) <- object@Model@dimNames[[mm]]

    ## fill in starting values
    m.user.idx <- object@Model@m.user.idx[[mm]]
    x.user.idx <- object@Model@x.user.idx[[mm]]
    GLIST[[mm]][m.user.idx] <- est.std[x.user.idx]

    ## class
    class(GLIST[[mm]]) <- c("matrix")


  }
  GLIST
}

