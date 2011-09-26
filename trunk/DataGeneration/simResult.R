setClass("simResult", 
	representation(
		Tag="character",
		Data="simData",
		Model="simModel",
		Replication="numeric",
                Estimates="data.frame",
		Fit="data.frame",
                SE="data.frame",
		Convergence="logical",
		Seed="numeric")
)

result.object <- function(simData, simModel.l, NRep, seed = 123321, silent=FALSE) {
	#Tag <- simData@Tag
        ###Check whether simModel analyze the same number of variables and the same tag
	#if(Tag != simModel@Tag) stop("simData and simModel do not have the same tag")
	Fit.l <- NULL 
        Estimates.l <- NULL # We need them. Trut me (Sunthud).
        SE.l <- NULL
        Convergence <- NULL
	set.seed(seed)
        if(!silent) cat(1, "\n")
        data <- run(SimData)
        SimAnalysis.l <- lapply(simModel.l, run, Data=data)
        # If we have complete data, simAnalysis provides 1 object.
        # If we have missing data and want to try different analyses (complete, MI, FIML), simAnalysis provides multiple objects.
        
        Estimates.l[[1]] <- lapply(SimAnalysis.l, function(object) object@Estimates) # It is list of list (Inner list is different model, Outer list is different replications).
        SE.l[[1]] <- lapply(SimAnalysis.l, function(object) object@SE)
        Fit.l[[1]] <- lapply(SimAnalysis.l, function(object) object@Fit)
        Convergence[[1]] <- lapply(SimAnalysis.l, function(object) object@Convergence)
        
	for(i in 2:NRep) {
          if(!silent) cat(i, "\n")
          data <- run(SimData)
          SimAnalysis.l <- lapply(simModel.l, run, Data=data)
          Estimates.l[[i]] <- lapply(SimAnalysis.l, function(object) object@Estimates) # It is list of list (Inner list is different model, Outer list is different replications).
          SE.l[[i]] <- lapply(SimAnalysis.l, function(object) object@SE)
          Fit.l[[i]] <- lapply(SimAnalysis.l, function(object) object@Fit)
          Convergence[[1]] <- lapply(SimAnalysis.l, function(object) object@Convergence)
	}
        
        Result.l <- NULL
        
        for(i in 1:length(simModel.l)) {
          Model.Estimate <- sapply(Estimates.l, function(object) object[[i]]) # Object is the inner list. We want to extract element i (model i) from the inner list. The sapply function will go to each replication.
          Model.SE <- sapply(SE.l, function(object) object[[i]])
          Model.Fit <- sapply(Fit.l, function(object) object[[i]]) 
          Model.Convergence <- sapply(Convergence.l, function(object) object[[i]])
          
          Result.l[[i]] <- new("simResult", Tag=Tag, Data=simData, Model=simModel, Replication=NRep,
                      Estimates=as.data.frame(t(Model.Estimate)), SE=as.data.frame(t(Model.SE)), Fit=as.data.frame(t(Model.Fit)),
                      Convergence=as.data.frame(t(Model.Convergence)), Seed=seed)
        }
        if(length(Result.l) == 1) unlist(Result.l)
	return <- Result.l
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
	

