runRep <- function(dataT, simModel, simMissing=new("NullSimMissing"), seed=123321, silent=FALSE) {
	
	modelType <- simModel@modelType
    param <- NULL
	coef <- NA
    se <- NA
    fit <- NA
	FMI1 <- NULL
	FMI2 <- NULL
	converged <- FALSE
	set.seed(seed)

         # if(!silent) cat(i, "\n")
        data.mis <- NULL  
        # Get Data
        if(class(dataT) == "SimDataOut") {
            data.mis <-dataT@data
		} else {
            data.mis <- dataT
        }


		 temp <- NULL
          #Impute missing and run results 
           if(!is(simMissing, "NullSimMissing") && simMissing@numImps > 0) {
              if(silent) {
                 invisible(capture.output(suppressMessages(try(temp <- runMI(data.mis,simModel,simMissing@numImps,simMissing@impMethod), silent=TRUE))))
              } else {
                        try(temp <- runMI(data.mis,simModel,simMissing@numImps,simMissing@impMethod))
              } 
              } else{
          if(silent) {
            invisible(capture.output(suppressMessages(try(temp <- run(object=simModel, data=data.mis), silent=TRUE))))
                 #tryCatch(temp <- run(simModel, data), error=function(e) {print("Error")})
          } else {
            try(temp <- run(object=simModel, data=data.mis))
          }
          }
          

          if(!is.null(temp)) {
            converged <- temp@converged			
            Labels <- make.labels(temp@param, "OpenMx") #As a quick default to use OpenMx
            coef<- vectorize.object(temp@coef, Labels)
            se <- vectorize.object(temp@se, Labels)
            fit <- temp@fit
			if(is(temp, "SimModelMIOut")) {
				#Can we make vectorize object work with simModelOutMI too?
				FMI1 <- vectorize.object(temp@FMI1, Labels)
				FMI2 <- vectorize.object(temp@FMI2, Labels)
			}
            if(!converged) {
              coef <- NA
              se <- NA
              fit <- NA
            }

            if(!is.null(dataT) && !is.null.object(dataT@paramOut)) {
              if(converged) {
                param <- vectorize.object(dataT@paramOut, Labels)
              } else {
                param <- NA
              }
            }
          }
	       

	#We need to output FMI also when there is missing information....
  Result <- list(coef=coef, se=se, fit=fit, converged=converged, param=param, FMI1=FMI1, FMI2=FMI2)
	return <- Result
}

