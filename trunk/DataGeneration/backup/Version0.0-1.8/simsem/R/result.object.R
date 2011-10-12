
result.object <- function(simData, simModel, NRep, seed = 123321, silent=FALSE) {
	set.seed(seed)
	Tag <- simData@Tag
	Fit.l <- NULL 
	Estimates.l <- NULL # We need them. Trut me (Sunthud).
	SE.l <- NULL
	Convergence.l <- NULL
	for(i in 1:NRep) {
        if(!silent) cat(i, "\n")
		data <- run(simData)
		temp <- NULL
		try(temp <- run(simModel, data))
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
	Result <- new("simResult", Tag=Tag, Replication=NRep, Estimates=Estimates, SE=SE, Fit=Fit, Convergence=Convergence, Seed=seed)
	return <- Result
}
