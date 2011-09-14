setClass("simResult", 
	representation(
		Tag="character",
		Data="simData",
		Model="simModel",
		Replication="numeric",
		Output="data.frame",
		Convergence="numeric",
		Seed="numeric")
)

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

find.fit.indices.OpenMx <- function(indiv.result) {
	temp.result <- summary(indiv.result)
	Chi <- temp.result$Chi
	p <- temp.result$p
	AIC <- temp.result$AIC.Mx
	BIC <- temp.result$BIC.Mx
	degree.freedom <- temp.result$degreesOfFreedom
	ni <- dim(temp.result$data$Model.data$cov)[1]
	no <- temp.result$numObs
	temp <- ((Chi / degree.freedom) - 1) / (no - 1)
	if(temp < 0) temp <- 0
	RMSEA <- sqrt(temp)
	result <- list(Chi = Chi, p.Chi = p, AIC = AIC, BIC = BIC, RMSEA = RMSEA)
	return(result)
}

# find.fit.indices <- function(simResult) {
	# output <- matrix(NA, nrow = simResult@Replication, ncol = 5)
	# temp.result <- NULL
	# colnames(output) <- c("Chi", "p", "AIC", "BIC", "RMSEA")
	# for(i in 1:simResult@Replication) {
		# indiv.result <- simResult@Output[[i]]
		# if(is.null(indiv.result)) {
			# output[i,] <- NA
		# } else {
			# indiv.result <- summary(indiv.result)
			# temp.result <- indiv.result
			# output[i, 1] <- indiv.result$Chi
			# output[i, 2] <- indiv.result$p
			# output[i, 3] <- indiv.result$AIC.Mx
			# output[i, 4] <- indiv.result$BIC.Mx
		# }
	# }
	# degree.freedom <- temp.result$degreesOfFreedom
	# ni <- dim(temp.result$data$Model.data$cov)[1]
	# estimated.parameters <- temp.result$estimatedParameters
	# no <- temp.result$numObs
	# temp <- rep(NA, simResult@Replication)
	# for(i in 1:simResult@Replication) {
		# temp[i] <- ((output[i, 1] / degree.freedom) - 1) / (no - 1)
		# if(temp[i] < 0) temp[i] <- 0
		# output[i,5] <- sqrt(temp[i])
	# }
	# return(as.data.frame(output))
# }

find.cutoff <- function(simResult, percentile, reverse = FALSE) {
	if(reverse) percentile <- 1 - percentile
	if(!is.matrix(simResult)) {
		Result <- simResult@Output
	} else {
		Result <- simResult
	}
	temp <- rep(NA, 8)
	temp <- apply(Result, 2, quantile, probs = percentile, na.rm = TRUE)
	temp[6] <- quantile(Result[,6], 1 - percentile, na.rm = TRUE)
	temp[7] <- quantile(Result[,7], 1 - percentile, na.rm = TRUE)
	return(temp)
}

visualize <- function(simResult, percentile=NULL, reverse = FALSE) {
	cutoff <- rep(NA, 7)
	if(!is.null(percentile)) {
		if(reverse) percentile <- 1 - percentile
		cutoff <- find.cutoff(simResult, percentile)[-2]
	}
	if(!is.matrix(simResult)) {
		Result <- simResult@Output[,-2]
	} else {
		Result <- simResult[,-2]
	}
	k <- 0
	for(i in 1:ncol(Result)) {
		if(!is.na.vector(Result[,i])) k = k + 1
	}
	obj <- par(mfrow = c(2, ceiling(k/2)))
	Kept.Fit <- c("Chi-square", "AIC", "BIC", "RMSEA", "CFI", "TLI", "SRMR")
	for(i in 1:ncol(Result)) {
		if(!is.na.vector(Result[,i])) { k = k + 1
			hist(Result[,i], main = Kept.Fit[i], breaks = 10, col="yellow", xlab = "value")
			if(!is.null(percentile)) abline(v = cutoff[i], col="red")
		}
	}
	par(obj)
}

is.na.vector <- function(vec) {
	k <- length(vec)
	match <- sum(is.na(vec))
	return(k == match)
}

pvalue<-function(x,vector, reverse =FALSE){
	if(reverse) {
		return(mean(x<=vector, na.rm = TRUE))
	} else {
		return(mean(x>=vector, na.rm = TRUE))
	}
}

find.power <- function(Result.alternative, cutoff, reverse = FALSE) {
	#browser()
	if(!is.matrix(Result.alternative)) {
		output <- Result.alternative@Output
	} else {
		output <- Result.alternative
	}
	Chi = pvalue(output[, 1], cutoff["Chi"], reverse)
	p = pvalue(output[, 2], cutoff["p"], reverse)
	AIC = pvalue(output[,3], cutoff["AIC"], reverse)
	BIC = pvalue(output[,4], cutoff["BIC"], reverse)
	RMSEA = pvalue(output[, 5], cutoff["RMSEA"], reverse)
	CFI = 1 - pvalue(output[, 6], cutoff["CFI"], reverse)
	TLI = 1 - pvalue(output[, 7], cutoff["TLI"], reverse)
	SRMR = pvalue(output[, 8], cutoff["SRMR"], reverse)
	result <- c(Chi, p, AIC, BIC, RMSEA, CFI, TLI, SRMR)
	names(result) <- names(cutoff)
	return(result)
}

plotOverlappingHist <- function(a, b, colors=c("red","blue","purple"),
                                breaks=NULL, xlim=NULL, ylim=NULL, main=NULL, xlab=NULL, percentile = NULL,
								find.power = FALSE){

	if(!is.matrix(a)) a <- a@Output
	if(!is.matrix(b)) b <- b@Output
	
	ahist=NULL
  bhist=NULL

  if(!(is.null(breaks))){
    ahist=hist(a,breaks=breaks,plot=F)
    bhist=hist(b,breaks=breaks,plot=F)
  } else {
    ahist=hist(a,plot=F)
    bhist=hist(b,plot=F)

    dist = ahist$breaks[2]-ahist$breaks[1]
    breaks = seq(min(ahist$breaks,bhist$breaks),max(ahist$breaks,bhist$breaks),dist)

    ahist=hist(a,breaks=breaks,plot=F)
    bhist=hist(b,breaks=breaks,plot=F)
  }

  if(is.null(xlim)){
    xlim = c(min(ahist$breaks,bhist$breaks),max(ahist$breaks,bhist$breaks))
  }

  if(is.null(ylim)){
    ylim = c(0,max(ahist$counts,bhist$counts))
  }

  overlap = ahist
  for(i in 1:length(overlap$counts)){
    if(ahist$counts[i] > 0 & bhist$counts[i] > 0){
      overlap$counts[i] = min(ahist$counts[i],bhist$counts[i])
    } else {
      overlap$counts[i] = 0
    }
  }

  plot(ahist, xlim=xlim, ylim=ylim, col=colors[1], main=main, xlab=xlab)
  plot(bhist, xlim=xlim, ylim=ylim, col=colors[2], add=T)
  plot(overlap, xlim=xlim, ylim=ylim, col=colors[3], add=T)
  if(!is.null(percentile)) {
	cutoff1 <- quantile(a, percentile, na.rm = TRUE)
	cutoff2 <- quantile(b, 1 - percentile, na.rm = TRUE)
	abline(v = cutoff1)
	if(find.power == FALSE) abline(v = cutoff2)
  }
}
	

