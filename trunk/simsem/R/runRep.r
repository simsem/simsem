runRep <- function(object, simData, simModel, simMissing=new("NullSimMissing"), silent=FALSE) {
	modelType <- simModel@modelType
    param <- NULL
	coef <- NA
    se <- NA
    fit <- NA
	std <- NA
	FMI1 <- NULL
	FMI2 <- NULL
	converged <- FALSE
	seed <- object[[2]]
	obj <- object[[1]]
	set.seed(seed)
	data.mis <- NULL 
	if(class(obj) == "list") {
		data.mis <- createData(obj, simData@n, simData, dataOnly=FALSE)
	} else {
		data.mis <- obj
	} 
    #if(class(dataT) == "SimDataOut") {
    #    data.mis <-dataT@data
	#} else {
    #    data.mis <- dataT
    #}

	if(!is(simMissing, "NullSimMissing")) {
		data.mis <- run(simMissing, data.mis)
		#data.mis <- imposeMissing(data.mis, covs=simMissing@covs, pmMCAR=simMissing@pmMCAR,
		#	pmMAR=simMissing@pmMAR, nforms=simMissing@nforms,
		#	itemGroups=simMissing@itemGroups, twoMethod=simMissing@twoMethod)
	} 

	temp <- NULL
          #Impute missing and run results 
    if(!is(simMissing, "NullSimMissing") && simMissing@numImps > 0) {
        if(silent) {
			invisible(capture.output(suppressMessages(try(temp <- run(object=simModel, data=data.mis, simMissing=simMissing), silent=TRUE))))
                 #invisible(capture.output(suppressMessages(try(temp <- runMI(data.mis,simModel,simMissing@numImps,simMissing@impMethod), silent=TRUE))))
        } else {
			try(temp <- run(object=simModel, data=data.mis, simMissing=simMissing), silent=TRUE)
                  #      try(temp <- runMI(data.mis,simModel,simMissing@numImps,simMissing@impMethod))
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
		param <- NA
		Labels <- make.labels(temp@param, "OpenMx") #As a quick default to use OpenMx
		if(converged) {
			coef<- vectorize.object(temp@coef, Labels)
			se <- vectorize.object(temp@se, Labels)
			fit <- temp@fit
			stdSet <- standardize(temp)
			std <- vectorize.object(stdSet, Labels)
			if(is(temp, "SimModelMIOut")) {
				#Can we make vectorize object work with simModelOutMI too?
				FMI1 <- vectorize.object(temp@FMI1, Labels)
				FMI2 <- vectorize.object(temp@FMI2, Labels)
			}
			if(!is.null.object(temp@paramValue)) {
				param <- vectorize.object(temp@paramValue, Labels)
			} else {
				param <- NA
			}
		} 
    } else {
		if(!is.null(data.mis) && is(data.mis, "SimDataOut")) param <- NA
	}
	Result <- list(coef=coef, se=se, fit=fit, converged=converged, param=param, FMI1=FMI1, FMI2=FMI2, std=std)
	return <- Result
}

