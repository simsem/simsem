
simResult <- function(simData, simModel, nRep, seed = 123321, silent=FALSE) {
	set.seed(seed)
	modelType <- simModel@modelType
	fit.l <- NULL 
	coef.l <- NULL # We need them. Trust me (Sunthud).
	se.l <- NULL
	converged.l <- NULL
	for(i in 1:nRep) {
        if(!silent) cat(i, "\n")
         if(class(simData) == "SimData") {
			data <- run(simData)
         } else if(is.list(simData)) {
			data <- simData[[i]]
         } else {
            data <- simData
         }  
		temp <- NULL
		if(silent) {
			invisible(capture.output(suppressMessages(try(temp <- run(simModel, data), silent=TRUE))))
			#tryCatch(temp <- run(simModel, data), error=function(e) {print("Error")})
		} else {
			try(temp <- run(simModel, data))
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
		}
	}
	coef <- as.data.frame(do.call(rbind, coef.l))
	se <- as.data.frame(do.call(rbind, se.l))
	fit <- as.data.frame(do.call(rbind, fit.l))
	converged <- as.vector(unlist(converged.l))
	Result <- new("SimResult", modelType=modelType, nRep=nRep, coef=coef, se=se, fit=fit, converged=converged, seed=seed)
	return <- Result
}
