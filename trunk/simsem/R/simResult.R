

simResult <- function(nRep=NULL, objData=NULL, objModel=NULL, objMissing=new("NullSimMissing"), seed = 123321, silent=FALSE, multicore=FALSE, cluster=FALSE, numProc=NULL, n=NULL, pmMCAR=NULL, pmMAR=NULL, objSet=NULL) {
	set.seed(seed)
	if(is.null(nRep)) {
		if(!is.vector(n)) stop("Please specify the number of replications")
		if(!is.null(pmMCAR) && !is.vector(pmMCAR)) stop("Please specify the number of replications")
		if(!is.null(pmMAR) && !is.vector(pmMAR)) stop("Please specify the number of replications")
		usedMCAR <- NULL
		usedMAR <- NULL
		ifelse(is.null(pmMCAR), usedMCAR <- 1, usedMCAR <- pmMCAR)
		ifelse(is.null(pmMAR), usedMAR <- 1, usedMAR <- pmMAR)
		out <- expand.grid(n, usedMCAR, usedMAR)
		n <- out[,1]
		if(!is.null(pmMCAR)) pmMCAR <- out[,2]
		if(!is.null(pmMAR)) pmMAR <- out[,3]
		nRep <- nrow(out)
	}
	if(!is.null(n)) {
		if(is(n, "VirtualDist")) {
			n <- round(run(n, nRep))
		} else if(is.vector(n)) { 
			if(length(n) != nRep) ifelse(length(n) > nRep, n <- sample(n, nRep, replace=TRUE), n <- sample(n, nRep))
		} else {
			stop("The n argument should be in a vector of numbers or distribution object only.")
		}
	}
	if(!is.null(pmMCAR)) {
		if(is(pmMCAR, "VirtualDist")) {
			pmMCAR <- run(pmMCAR, nRep)
		} else if(is.vector(pmMCAR)) {
			if(length(pmMCAR) != nRep) ifelse(length(pmMCAR) > nRep, pmMCAR <- sample(pmMCAR, nRep, replace=TRUE), pmMCAR <- sample(pmMCAR, nRep))
		} else {
			stop("The pmMCAR argument should be in a vector of numbers or distribution object only.")
		}
	}
	if(!is.null(pmMAR)) {
		if(is(pmMAR, "VirtualDist")) {
			pmMAR <- run(pmMAR, nRep)
		} else if(is.vector(pmMAR)) {
			if(length(pmMAR) != nRep) ifelse(length(pmMAR) > nRep, pmMAR <- sample(pmMAR, nRep, replace=TRUE), pmMAR <- sample(pmMAR, nRep))
		} else {
			stop("The pmMAR argument should be in a vector of numbers or distribution object only.")
		}
	}
	if(!is.null(objSet)) {
		if(is.null(n)) stop("If the SimSet is specified, the sample size (n) must be specified too.")
		ifelse(is.null(objData), objData <- simData(objSet, n[1]), stop("SimData and SimSet cannot be specified at the same time."))
		ifelse(is.null(objModel), objModel <- simModel(objSet), stop("SimModel and SimSet cannot be specified at the same time."))
	}
	modelType <- objModel@modelType
    param <- NULL
	object.l <- list()
	if(is.null(objData)) stop("Please provide a SimData object or a list of dataset in the objData argument")
	if(is.null(objModel)) stop("Please provide a SimModel object in the objModel argument")
	
    if(class(objData) == "SimData") {
		for(i in 1:nRep) {
			object.l[[i]] <- drawParameters(objData)
		}
    } else if(is.list(objData)) { 
		if(class(objData[[1]]) == "SimDataOut") {
			object.l <- objData
			if(!is.null(n) && ((n > objData@n) %in% TRUE)) stop("The specified n is greater than the number of cases provided.")
		} else if(is.matrix(objData[[1]])) {
			object.l <- lapply(objData, data.frame)
			if(!is.null(n) && ((n > nrow(objData[[1]])) %in% TRUE)) stop("The specified n is greater than the number of cases provided.")
		} else if(is.data.frame(objData[[1]])) {
			object.l <- objData
			if(!is.null(n) && ((n > nrow(objData[[1]])) %in% TRUE)) stop("The specified n is greater than the number of cases provided.")
		} else {
			stop("The list in the objData argument does not contain matrices or data frames.")
		}
    } else {
        stop("The objData argument is not a SimData class or a list of data frames.")
    }
	numseed <- as.list(round(sample(1:999999, nRep)))
	
	object2.l <- list()
	for(i in 1:length(object.l)) {
		object2.l[[i]] <- list()
		object2.l[[i]][[1]] <- object.l[[i]]
		object2.l[[i]][[2]] <- n[i]
		object2.l[[i]][[3]] <- pmMCAR[i]
		object2.l[[i]][[4]] <- pmMAR[i]
		object2.l[[i]][[5]] <- numseed[[i]]
	}
	
	if(multicore) {
		library(parallel)
		sys <- .Platform$OS.type
		if(is.null(numProc)) numProc <- detectCores()
		if(sys == "windows") {
			cl <- makeCluster(rep("localhost", numProc), type="SOCK")
			Result.l <- clusterApplyLB(cl, object2.l, runRep, objData=objData, objModel=objModel, objMissing=objMissing, silent=silent)	
			stopCluster(cl)
		} else {
			Result.l <- mclapply(object2.l, runRep, objData=objData, objModel=objModel, objMissing=objMissing, silent=silent, mc.cores=numProc)				
		}
	} else {
		Result.l <- lapply(object2.l, runRep, objData=objData, objModel=objModel, objMissing=objMissing, silent=silent)	
	}
	

	
	
	fit.l <- lapply(Result.l, function(object) {object$fit}) 
	coef.l <- lapply(Result.l, function(object) {object$coef})  
	se.l <- lapply(Result.l, function(object) {object$se}) 
	converged.l <- lapply(Result.l, function(object) {object$converged}) 
	param.l <- lapply(Result.l, function(object) {object$param})  
	FMI1.l <- lapply(Result.l, function(object) {object$FMI1}) 
	FMI2.l <- lapply(Result.l, function(object) {object$FMI2})
	std.l <- lapply(Result.l, function(object) {object$std})
	coef <- as.data.frame(do.call(rbind, coef.l))
	se <- as.data.frame(do.call(rbind, se.l))
	fit <- as.data.frame(do.call(rbind, fit.l))
	FMI1 <- as.data.frame(do.call(rbind, FMI1.l))
	FMI2 <- as.data.frame(do.call(rbind, FMI2.l))
	std <- as.data.frame(do.call(rbind, std.l))
	converged <- as.vector(unlist(converged.l))
	param <- new("NullDataFrame")
	FMI1 <- new("NullDataFrame")
	FMI2 <- new("NullDataFrame")
	if(!is.null(param.l)) {
		param <- as.data.frame(do.call(rbind, param.l))
		if(sum(dim(param)) == 0) param <- new("NullDataFrame")
		if(nrow(unique(param)) == 1) param <- unique(param)
	}
	if(!is.null(FMI1.l)) {
		FMI1 <- as.data.frame(do.call(rbind, FMI1.l))
		if(sum(dim(FMI1)) == 0) FMI1 <- new("NullDataFrame")
		if(nrow(unique(FMI1)) == 1) FMI1 <- unique(FMI1)
	}
	if(!is.null(FMI2.l)) {
		FMI2 <- as.data.frame(do.call(rbind, FMI2.l))
		if(sum(dim(FMI2)) == 0) FMI2 <- new("NullDataFrame")
		if(nrow(unique(FMI2)) == 1) FMI2 <- unique(FMI2)
	}
	if(is.null(n)) {
		if(class(objData) == "SimData") {
			if(is.null(n)) n <- objData@n
		} else if(is.list(objData)) { 
			if(class(objData[[1]]) == "SimDataOut") {
				n <- objData@n
			} else if(is.matrix(objData[[1]]) | is.data.frame(objData[[1]])) {
				n <- nrow(objData[[1]])
			} 
		} 
	}
	if(is.null(pmMCAR)) ifelse(is.null.object(objMissing), pmMCAR <- 0, pmMCAR <- objMissing@pmMCAR)
	if(is.null(pmMAR)) ifelse(is.null.object(objMissing), pmMAR <- 0, pmMAR <- objMissing@pmMAR)
	Result <- new("SimResult", modelType=modelType, nRep=nRep, coef=coef, se=se, fit=fit, converged=converged, 
		seed=seed, paramValue=param, FMI1=FMI1, FMI2=FMI2, stdCoef=std, n=n, pmMCAR=pmMCAR, pmMAR=pmMAR)
	return <- Result
}
