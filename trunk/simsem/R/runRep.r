runSIM <- function(simData, simModel, nrep, simMissing=NULL, seed = 123321, silent=FALSE) {
set.seed(seed)
numseed <- round(runif(nrep, 0, 1000000))
as.list(numseed)
result <- mapply(simData, runRep, simModel=simModel, simMissing=simMissing, seed=numseed, silent=silent)
#combine list together
}

runRep <- function(simData, simModel, simMissing=new("NullSimMissing"), seed=123321, silent=FALSE) {
	
	modelType <- simModel@modelType

        set.seed(seed)

         # if(!silent) cat(i, "\n")
          
        # Get Data
          if(class(simData) == "SimData") {
            data <- run(simData, dataOnly=FALSE)
          } else if(is.list(simData) && !is.data.frame(simData)) {
            data <- data.frame(simData)
          } else {
            data <- simData
          }

         # Impose / Impute Missing 
		 if(!is.null.object(simMissing)) {
          data.mis <- imposeMissing(data, covs=simMissing@covs, pmMCAR=simMissing@pmMCAR,
                                    pmMAR=simMissing@pmMAR, nforms=simMissing@nforms,
                                    itemGroups=simMissing@itemGroups, twoMethod=simMissing@twoMethod)
		} else {
			data.mis <- data
		}
		 temp <- NULL
          #Impute missing and run results 
           if(!is.null.object(simMissing)) {
              tempMI<-NULL
              if(silent) {
                 invisible(capture.output(suppressMessages(try(tempMI <- runMI(data.mis,simModel,simMissing@numImps,simMissing@impMethod)), silent=TRUE)))
              } else {
                        try(tempMI <- runMI(data.mis,simModel,simMissing@numImps,simMissing@impMethod))
              }
              temp <- new("SimResult", modelType=simModel@modelType,nRep=1, coef=tempMI[[1]],
                               se=tempMI[[2]], fit=tempMI[[3]], converged =!is.null(tempMI))
              } else{
          if(silent) {
            invisible(capture.output(suppressMessages(try(temp <- run(simModel, data.mis), silent=TRUE))))
                 #tryCatch(temp <- run(simModel, data), error=function(e) {print("Error")})
          } else {
            try(temp <- run(simModel, data.mis))
          }
          }
          
          if(!is.null(temp)) {
            converged <- temp@converged			
            Labels <- make.labels(temp@param, "OpenMx") #As a quick default to use OpenMx
            coef<- vectorize.object(temp@coef, Labels)
            se <- vectorize.object(temp@se, Labels)
            fit <- temp@fit

            if(!converged) {
              coef <- NA
              se <- NA
              fit <- NA
            }

            if(!is.null.object(temp@paramValue)) {
              if(converged) {
                param <- vectorize.object(temp@paramValue, Labels)
              } else {
                param <- NA
              }
            }
          }
	       

	
  Result <- list(coef=coef, se=se, fit=fit, converged=converged, param=param)
	return <- Result
}

