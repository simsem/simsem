runRep <- function(object, objData, objModel, objMissing=new("NullSimMissing"), silent=FALSE) {
	modelType <- objModel@modelType
    param <- NULL
	coef <- NA
    se <- NA
    fit <- NA
	std <- NA
	FMI1 <- NULL
	FMI2 <- NULL
	converged <- FALSE
	seed <- object[[5]]
	n <- object[[2]]
	if(is.null(n)) n <- objData@n
	pmMCAR <- object[[3]]
	pmMAR <- object[[4]]
	if(is.null.object(objMissing)) {
		if(!is.null(pmMAR) | !is.null(pmMCAR)) {
			if(is.null(pmMCAR)) pmMCAR <- 0
			if(is.null(pmMAR)) pmMAR <- 0
			objMissing <- simMissing(pmMCAR=pmMCAR, pmMAR=pmMAR)
		}
	}
	obj <- object[[1]]
	set.seed(seed)
	data.mis <- NULL 
	if(class(obj) == "list") {
		data.mis <- createData(obj, n, objData, dataOnly=FALSE)
	} else {
		data.mis <- obj
	} 
    #if(class(dataT) == "SimDataOut") {
    #    data.mis <-dataT@data
	#} else {
    #    data.mis <- dataT
    #}

	if(!is(objMissing, "NullSimMissing")) {
		data.mis <- run(objMissing, data.mis, pmMCAR=pmMCAR, pmMAR=pmMAR)
		#data.mis <- imposeMissing(data.mis, covs=objMissing@covs, pmMCAR=objMissing@pmMCAR,
		#	pmMAR=objMissing@pmMAR, nforms=objMissing@nforms,
		#	itemGroups=objMissing@itemGroups, twoMethod=objMissing@twoMethod)
	} 

	temp <- NULL
          #Impute missing and run results 
    if(!is(objMissing, "NullSimMissing") && objMissing@numImps > 0) {
        if(silent) {
			invisible(capture.output(suppressMessages(try(temp <- run(object=objModel, data=data.mis, simMissing=objMissing), silent=TRUE))))
                 #invisible(capture.output(suppressMessages(try(temp <- runMI(data.mis,objModel,objMissing@numImps,objMissing@impMethod), silent=TRUE))))
        } else {
			try(temp <- run(object=objModel, data=data.mis, simMissing=objMissing), silent=TRUE)
                  #      try(temp <- runMI(data.mis,objModel,objMissing@numImps,objMissing@impMethod))
        } 
    } else{
          if(silent) {
            invisible(capture.output(suppressMessages(try(temp <- run(object=objModel, data=data.mis), silent=TRUE))))
                 #tryCatch(temp <- run(objModel, data), error=function(e) {print("Error")})
          } else {
            try(temp <- run(object=objModel, data=data.mis))
          }
    }

	if(!is.null(temp)) {
		converged <- temp@converged
		param <- NA
		Labels <- makeLabels(temp@param, "OpenMx") #As a quick default to use OpenMx
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

