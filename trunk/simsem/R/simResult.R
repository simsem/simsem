

simResult <- function(nRep, simData, simModel, simMissing=new("NullSimMissing"), seed = 123321, silent=FALSE, multicore=FALSE, cluster=FALSE, numProc=NULL) {
	set.seed(seed)
	numseed <- as.list(sample(1:999999, nRep))
	if(multicore) {
		library(parallel)
		if(is.null(numProc)) numProc <- detectCores()
		cl <- makeCluster(rep("localhost", numProc), type="SOCK")
			if(is(simData, "SimData")) {
				Result.l <- parLapply(cl, numseed, runRep, simData=simData, simModel=simModel, simMissing=simMissing, silent=silent)	
			} else if(is(simData, "list")) {
				Result.l <- parLapply(cl, simData, runRep, simModel=simModel, simMissing=simMissing, seed=seed, silent=silent)	
			}
		stopCluster(cl)
	} else {
		if(is(simData, "SimData")) {
			Result.l <- lapply(numseed, runRep, simData=simData, simModel=simModel, simMissing=simMissing, silent=silent)	
		} else if(is(simData, "list")) {
			Result.l <- lapply(simData, runRep, simModel=simModel, simMissing=simMissing, seed=seed, silent=silent)	
		}  
	}
	
	
	modelType <- simModel@modelType
	fit.l <- lapply(Result.l, function(object) {object$fit}) 
	coef.l <- lapply(Result.l, function(object) {object$coef})  
	se.l <- lapply(Result.l, function(object) {object$se}) 
	converged.l <- lapply(Result.l, function(object) {object$converged}) 
	param.l <- lapply(Result.l, function(object) {object$param})  
  #Same here, we need to save FMI information
  FMI1.l <- lapply(Result.l, function(object) {object$FMI1}) 
	FMI2.l <- lapply(Result.l, function(object) {object$FMI2})
	coef <- as.data.frame(do.call(rbind, coef.l))
	se <- as.data.frame(do.call(rbind, se.l))
	fit <- as.data.frame(do.call(rbind, fit.l))
  FMI1 <- as.data.frame(do.call(rbind, FMI1.l))
  FMI2 <- as.data.frame(do.call(rbind, FMI2.l))
	converged <- as.vector(unlist(converged.l))
	param <- new("NullDataFrame")
	FMI1 <- new("NullDataFrame")
	FMI2 <- new("NullDataFrame")
	if(!is.null(param.l)) {
		param <- as.data.frame(do.call(rbind, param.l))
		if(sum(dim(param)) == 0) param <- new("NullDataFrame")
		if(nrow(unique(param)) == 1) param <- unique(param)
	}
	if(!is.null(FMI1.l)) {
		FMI1 <- as.data.frame(do.call(rbind, FMI1.l))
		if(sum(dim(FMI1)) == 0) FMI1 <- new("NullDataFrame")
		if(nrow(unique(FMI1)) == 1) FMI1 <- unique(FMI1)
	}
	if(!is.null(FMI2.l)) {
		FMI2 <- as.data.frame(do.call(rbind, FMI2.l))
		if(sum(dim(FMI2)) == 0) FMI2 <- new("NullDataFrame")
		if(nrow(unique(FMI2)) == 1) FMI2 <- unique(FMI2)
	}
  #SimResult needs informatin for FMI too...
	Result <- new("SimResult", modelType=modelType, nRep=nRep, coef=coef, se=se, fit=fit, converged=converged, 
		seed=seed, paramValue=param, FMI1=FMI1, FMI2=FMI2)
	return <- Result
}



       
#	for(i in 1:nRep) {
#          if(!silent) cat(i, "\n")
#          
        # Get Data
#          if(class(simData) == "SimData") {
#            data <- run(simData, dataOnly=FALSE)
#          } else if(is.list(simData) && !is.data.frame(simData)) {
#            data <- simData[[i]]
#          } else {
#            data <- simData
#          }
#         
#         temp <- NULL
#         
#         if(!is.null(simMissing)){
#         # Impose / Impute Missing 
#        
#          data.mis <- imposeMissing(data@data, covs=simMissing@covs, pmMCAR=simMissing@pmMCAR,
#                                    pmMAR=simMissing@pmMAR, nforms=simMissing@nforms,
#                                    itemGroups=simMissing@itemGroups, twoMethod=simMissing@twoMethod, timePoints=simMissing@timePoints)
#               }                     
#          
#          temp <- NULL#Impute missing and run results NEEED TO GET PARAMETER LABELS FROM runMI
#          
#          if(numImps>0) {
#              tempMI<-NULL
#              if(silent) {
#                 invisible(capture.output(suppressMessages(try(tempMI <- runMI(data.mis,simModel,simMissing@numImps,simMissing@impMethod)), silent=TRUE)))
#              } else {
#                        try(tempMI <- runMI(data.mis,simModel,simMissing@numImps,simMissing@impMethod))
#              }
#              temp <- new("SimResult", modelType=simModel@modelType,nRep=1, coef=as.data.frame(tempMI[[1]]),
#                               se=as.data.frame(tempMI[[2]]), fit=as.data.frame(t(tempMI[[3]])), converged =!is.null(tempMI))
#              } else{
#          if(silent) {
#            invisible(capture.output(suppressMessages(try(temp <- run(simModel, data), silent=TRUE))))
#                 #tryCatch(temp <- run(simModel, data), error=function(e) {print("Error")})
#          } else {
#            try(temp <- run(simModel, data))
#          }
#          }
#          
#          
#          if(!is.null(temp)) {
#            converged.l[[i]] <- temp@converged	     
#            Labels <- make.labels(temp@param, package=simModel@package) #As a quick default to use OpenMx
#            coef.l[[i]] <- vectorize.object(temp@coef, Labels)
#            se.l[[i]] <- vectorize.object(temp@se, Labels)
#            fit.l[[i]] <- temp@fit
#
#            if(!converged.l[[i]]) {
#              coef.l[[i]] <- NA
#              se.l[[i]] <- NA
#              fit.l[[i]] <- NA
#            }
#
#            if(!is.null.object(temp@paramValue)) {
#              if(converged.l[[i]]) {
#                param.l[[i]] <- vectorize.object(temp@paramValue, Labels)
#              } else {
#                param.l[[i]] <- NA
#              }
#            }
#          }
#	}