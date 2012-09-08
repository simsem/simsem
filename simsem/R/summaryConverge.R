# summaryMisspec: This function will summarize the obtained fit indices and generate a data frame.

summaryConverge <- function(object) {
	result <- list()
	converged <- object@converged
	numnonconverged <- sum(!converged)
	if(numnonconverged == 0) stop("You are good! All replications were converged!")
	result <- c(result, list(Converged = c(num.converged=sum(converged), num.nonconverged=numnonconverged)))
	n <- object@n
	pmMCAR <- object@pmMCAR
	pmMAR <- object@pmMAR
	paramValue <- object@paramValue
	misspecValue <- object@misspecValue
	popFit <- object@popFit
	if(length(unique(n)) > 1) {
		temp1 <- n[converged]
		temp2 <- n[!converged]
		resultTemp <- c(mean.convergence = mean(temp1), sd.convergence = sd(temp1), mean.nonconvergence = mean(temp2), sd.nonconvergence = sd(temp2), diff.mean = mean(temp1)-mean(temp2), diff.sd=sd(temp1)-sd(temp2))
		result <- c(result, list(n=resultTemp))
	}
	if(length(unique(pmMCAR)) > 1) {
		temp1 <- pmMCAR[converged]
		temp2 <- pmMCAR[!converged]
		resultTemp <- c(mean.convergence = mean(temp1), sd.convergence = sd(temp1), mean.nonconvergence = mean(temp2), sd.nonconvergence = sd(temp2), diff.mean = mean(temp1)-mean(temp2), diff.sd=sd(temp1)-sd(temp2))
		result <- c(result, list(pmMCAR=resultTemp))	
	}
	if(length(unique(pmMAR)) > 1) {
		temp1 <- pmMAR[converged]
		temp2 <- pmMAR[!converged]
		resultTemp <- c(mean.convergence = mean(temp1), sd.convergence = sd(temp1), mean.nonconvergence = mean(temp2), sd.nonconvergence = sd(temp2), diff.mean = mean(temp1)-mean(temp2), diff.sd=sd(temp1)-sd(temp2))
		result <- c(result, list(pmMAR=resultTemp))	
	}
	temp1 <- paramValue[converged,]
	temp2 <- paramValue[!converged,]
	resultTemp <- cbind(mean.convergence = apply(temp1, 2, mean), sd.convergence = apply(temp1, 2, sd), mean.nonconvergence = apply(temp2, 2, mean), sd.nonconvergence = apply(temp2, 2, sd))
	resultTemp <- cbind(resultTemp, diff.mean=resultTemp[,1]-resultTemp[,3], diff.sd=resultTemp[,2]-resultTemp[,4])
	result <- c(result, list(paramValue=resultTemp))	
	if(!all(dim(misspecValue) == 0)) {
		temp1 <- misspecValue[converged,]
		temp2 <- misspecValue[!converged,]
		resultTemp <- cbind(mean.convergence = apply(temp1, 2, mean), sd.convergence = apply(temp1, 2, sd), mean.nonconvergence = apply(temp2, 2, mean), sd.nonconvergence = apply(temp2, 2, sd))
		resultTemp <- cbind(resultTemp, diff.mean=resultTemp[,1]-resultTemp[,3], diff.sd=resultTemp[,2]-resultTemp[,4])
		result <- c(result, list(misspecValue=resultTemp))	
	}
	if(!all(dim(popFit) == 0)) {
		temp1 <- popFit[converged,]
		temp2 <- popFit[!converged,]
		resultTemp <- cbind(mean.convergence = apply(temp1, 2, mean), sd.convergence = apply(temp1, 2, sd), mean.nonconvergence = apply(temp2, 2, mean), sd.nonconvergence = apply(temp2, 2, sd))
		resultTemp <- cbind(resultTemp, diff.mean=resultTemp[,1]-resultTemp[,3], diff.sd=resultTemp[,2]-resultTemp[,4])
		result <- c(result, list(popFit=resultTemp))	
	}
	return(result)
}
