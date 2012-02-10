runSIM <- function(simData, simModel, nrep, simMissing=NULL, seed = 123321, silent=FALSE) {
set.seed(seed)
numseed <- round(runif(nrep, 0, 1000000))
as.list(numseed)
result <- mapply(simData, runRep, simModel=simModel, simMissing=simMissing, seed=numseed, silent=silent)
#combine list together
}

runRep <- function(simData, simModel, simMissing=new("NullSimMissing"), seed=123321, silent=FALSE) {
	
	modelType <- simModel@modelType
                param <- NULL
        set.seed(seed)

         # if(!silent) cat(i, "\n")
          
        # Get Data
          if(class(simData) == "SimData") {
            #We need just the data to run through impose missing. This saves the paramter estimates and then only uses the data
            dataT <- run(simData, dataOnly=FALSE)
            data<-dataT@data
          } else if(is.matrix(simData)) {
            data <- data.frame(simData)
          } else if(is.data.frame(simData)) {
            data <- simData
          } else {
            stop("The simData argument is not a SimData class, a matrix, or a data frame.")
          }

         # Impose / Impute Missing 
		 if(!is(simMissing, "NullSimMissing")) {
          data.mis <- imposeMissing(data, covs=simMissing@covs, pmMCAR=simMissing@pmMCAR,
                                    pmMAR=simMissing@pmMAR, nforms=simMissing@nforms,
                                    itemGroups=simMissing@itemGroups, twoMethod=simMissing@twoMethod)
		} else {
			data.mis <- data
		}
		 temp <- NULL
          #Impute missing and run results 
           if(!is(simMissing, "NullSimMissing") && simMissing@numImps > 0) {
              if(silent) {
                 invisible(capture.output(suppressMessages(try(temp <- runMI(data.mis,simModel,simMissing@numImps,simMissing@impMethod)), silent=TRUE)))
              } else {
                        try(temp <- runMI(data.mis,simModel,simMissing@numImps,simMissing@impMethod))
              } 
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
            #Can we make vectorize object work with simModelOutMI too?
            #FMI1 <- vectorize.object(temp@FMI1, Labels)
            #FMI2 <- vectorize.object(tempFMI2, Lables)

            if(!converged) {
              coef <- NA
              se <- NA
              fit <- NA
            }

            if(!is.null.object(dataT@paramOut)) {
              if(converged) {
                param <- vectorize.object(dataT@paramOut, Labels)
              } else {
                param <- NA
              }
            }
          }
	       

	#We need to output FMI also when there is missing information....
  Result <- list(coef=coef, se=se, fit=fit, converged=converged, param=param)# FMI1=FMI1, FMI2=FMI2)
	return <- Result
}

