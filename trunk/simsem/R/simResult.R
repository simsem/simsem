
simResult <- function(simData, simModel, simMissing=NULL, nRep, seed = 123321, silent=FALSE) {
	set.seed(seed)
	modelType <- simModel@modelType
	fit.l <- NULL 
	coef.l <- NULL # We need them. Trust me (Sunthud).
	se.l <- NULL
	converged.l <- NULL
	param.l <- NULL
        
	for(i in 1:nRep) {
          if(!silent) cat(i, "\n")
          
        # Get Data
          if(class(simData) == "SimData") {
            data <- run(simData, dataOnly=FALSE)
          } else if(is.list(simData) && !is.data.frame(simData)) {
            data <- simData[[i]]
          } else {
            data <- simData
          }

         # Impose / Impute Missing 
         # Still need impute missing. I'm working on it (Alex)
          data.mis <- imposeMissing(data, covs=simMissing@covs, pmMCAR=simMissing@pmMCAR,
                                    pmMAR=simMissing@pmMAR, nforms=simMissing@nforms,
                                    itemGroups=simMissing@itemGroups, twoMethod=simMissing@twoMethod)
          temp <- NULL
          #Impute missing and run results NEEED TO GET PARAMETER LABELS FROM runMI
           if(simMissing@numImps==NULL) {
              tempMI<-NULL
              if(silent) {
                 invisible(capture.output(suppressMessages(try(tempMI <- runMI(data.mis,simModel,simMissing@numImps,simMissing@impMethod)), silent=TRUE)))
              } else {
                        try(tempMI <- runMI(data.mis,simModel,simMissing@numImps,simMissing@impMethod))
              }
              temp <- new("SimResult", modelType=simModel@modelType,nRep=1, coef=tempMI[[1]],
                               se=tempMI[[2]], fit=tempMI[[2]], converged =!is.null(tempMI))
              } else{
          if(silent) {
            invisible(capture.output(suppressMessages(try(temp <- run(simModel, data), silent=TRUE))))
                 #tryCatch(temp <- run(simModel, data), error=function(e) {print("Error")})
          } else {
            try(temp <- run(simModel, data))
          }
          }
          
          if(!is.null(temp)) {
            converged.l[[i]] <- temp@converged			
            Labels <- make.labels(temp@param, "OpenMx") #As a quick default to use OpenMx
            coef.l[[i]] <- vectorize.object(temp@coef, Labels)
            se.l[[i]] <- vectorize.object(temp@se, Labels)
            fit.l[[i]] <- temp@fit

            if(!converged.l[[i]]) {
              coef.l[[i]] <- NA
              se.l[[i]] <- NA
              fit.l[[i]] <- NA
            }

            if(!is.null.object(temp@paramValue)) {
              if(converged.l[[i]]) {
                param.l[[i]] <- vectorize.object(temp@paramValue, Labels)
              } else {
                param.l[[i]] <- NA
              }
            }
          }
	}
        
	coef <- as.data.frame(do.call(rbind, coef.l))
	se <- as.data.frame(do.call(rbind, se.l))
	fit <- as.data.frame(do.call(rbind, fit.l))
	converged <- as.vector(unlist(converged.l))
	param <- new("NullDataFrame")
	if(!is.null(param.l)) {
		param <- as.data.frame(do.call(rbind, param.l))
		if(nrow(unique(param)) == 1) param <- unique(param)
	}
	Result <- new("SimResult", modelType=modelType, nRep=nRep, coef=coef, se=se, fit=fit, converged=converged, seed=seed, paramValue=param)
	return <- Result
}
