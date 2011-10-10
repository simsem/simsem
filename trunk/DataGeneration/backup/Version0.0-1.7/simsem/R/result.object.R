result.object <- function(simData, simModel, NRep, seed = 123321, silent=FALSE) {
	Tag <- simData@Tag
	if(Tag != simModel@Tag) stop("simData and simModel do not have the same tag")
	Kept.Fit <- c("Chi", "p", "AIC", "BIC", "RMSEA", "CFI", "TLI", "SRMR")
	Output <- matrix(NA, NRep, length(Kept.Fit))
	colnames(Output) <- Kept.Fit
	Convergence <- 0
	set.seed(seed)
	for(i in 1:NRep) {
		if(!silent) cat(i, "\n")
		data <- run(simData)
		temp <- NULL
		try(temp <- run(simModel, data))
		Convergence <- Convergence + temp$Converged
		if(!is.null(temp) & temp$Converged) {
			fit <- NULL
			if(simModel@Program == "OpenMx") {
				fit <- find.fit.indices.OpenMx(temp)
			} else if(simModel@Program == "lavaan") {
				fit <- temp$Summary
				Output[i, 6] <- fit$CFI
				Output[i, 7] <- fit$TLI
				Output[i, 8] <- fit$SRMR
			}
			Output[i, 1] <- fit$Chi
			Output[i, 2] <- fit$p.Chi
			Output[i, 3] <- fit$AIC
			Output[i, 4] <- fit$BIC
			Output[i, 5] <- fit$RMSEA
		}
	}
	Output <- as.data.frame(Output)
	Result <- new("simResult", Tag=Tag, Data=simData, Model=simModel, Replication=NRep, Output=Output, Convergence=Convergence, Seed=seed)
	return <- Result
}
