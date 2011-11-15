
simResult <- function(SimData, SimModel, NRep, seed = 123321, silent=FALSE) {
	set.seed(seed)
	modelType <- SimModel@modelType
	Fit.l <- NULL 
	Estimates.l <- NULL # We need them. Trust me (Sunthud).
	SE.l <- NULL
	Convergence.l <- NULL
	for(i in 1:NRep) {
        if(!silent) cat(i, "\n")
         if(class(SimData) == "SimData") {
		data <- run(SimData)
         } else if(is.list(SimData)) {
		 data <- SimData[[i]]
         } else {
                   data <- SimData
         }
                
		temp <- NULL
		try(temp <- run(SimModel, data))
		if(!is.null(temp)) {
			Convergence.l[[i]] <- temp@Convergence			
			Labels <- make.labels(temp@Parameters, "OpenMx") #As a quick default to use OpenMx
			Estimates.l[[i]] <- vectorize.object(temp@Estimates, Labels)
			SE.l[[i]] <- vectorize.object(temp@SE, Labels)
			Fit.l[[i]] <- temp@Fit
			if(!Convergence.l[[i]]) {
				Estimates.l[[i]] <- NA
				SE.l[[i]] <- NA
				Fit.l[[i]] <- NA
			}
		}
	}
	Estimates <- as.data.frame(do.call(rbind, Estimates.l))
	SE <- as.data.frame(do.call(rbind, SE.l))
	Fit <- as.data.frame(do.call(rbind, Fit.l))
	Convergence <- as.vector(unlist(Convergence.l))
	Result <- new("SimResult", modelType=modelType, Replication=NRep, Estimates=Estimates, SE=SE, Fit=Fit, Convergence=Convergence, Seed=seed)
	return <- Result
}
