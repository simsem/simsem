# summaryFit: This function will summarize the obtained fit indices and generate a data frame.

setMethod("summaryFit", signature(object = "SimResult"), definition = function(object, alpha = NULL) {
        cleanObj <- clean(object)
		usedFit <- getKeywords()$usedFit

	condition <- c(length(unique(object@pmMCAR)) > 1, length(unique(object@pmMAR)) > 1, length(unique(object@n)) > 1)
	if(any(condition)) {
		if (is.null(alpha)) 
			alpha <- 0.05
		values <- list()
		ifelse(condition[3], values[[3]] <- round(seq(min(object@n), max(object@n), length.out=5)), values[[3]] <- NA)
		ifelse(condition[1], values[[1]] <- seq(min(object@pmMCAR), max(object@pmMCAR), length.out=5), values[[1]] <- NA)
		ifelse(condition[2], values[[2]] <- seq(min(object@pmMAR), max(object@pmMAR), length.out=5), values[[2]] <- NA)
		m <- do.call(expand.grid, values)
		FUN <- function(vec, obj, alpha, usedFit) getCutoff(obj, alpha, revDirec = FALSE, usedFit = usedFit, nVal=vec[3], pmMCARval=vec[1], pmMARval=vec[2])
		cutoffs <- sapply(as.data.frame(t(m)), FUN, obj=object, alpha=alpha, usedFit=usedFit)
		mSelect <- as.matrix(m[,condition])
		colnames(mSelect) <- c("%MCAR", "%MAR", "N")[condition]
		result <- data.frame(mSelect, t(cutoffs))
		rownames(result) <- NULL
	} else {
		if (is.null(alpha)) 
			alpha <- c(0.1, 0.05, 0.01, 0.001)
		cutoffs <- sapply(alpha, getCutoff, object = cleanObj, usedFit = usedFit)
		cutoffs <- matrix(as.numeric(cutoffs), length(usedFit), length(alpha))
		if (ncol(as.matrix(cutoffs)) == 1) {
			cutoffs <- t(cutoffs)
			#rownames(cutoffs) <- usedFit
		}
		
		fit <- as.data.frame(cleanObj@fit[,usedFit])
		meanfit <- apply(fit, 2, mean, na.rm=TRUE)
		result <- cbind(cutoffs, meanfit)
		colnames(result) <- c(alpha, "Mean")
		rownames(result)<-usedFit
		names(dimnames(result)) <- c("Fit Indices", "Alpha")
		#print(as.data.frame(cutoffs))
	}
		
    return(result)
})


# setMethod("summaryFit", signature(object = "SimResultParam"), definition = function(object, digits=3) {
	# if(isNullObject(object@fit)) {
		# stop("This object does not have any model misspecification.")
	# } else {
		# quantileValue <- c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)
		# result <- round(apply(object@fit, 2, quantile, quantileValue), digits)
		# names(dimnames(result)) <- c("Fit Indices", "Quantile")
		# fitAverage <- colMeans(object@fit, na.rm = TRUE)
		# fitSE <- sapply(object@fit, sd, na.rm = TRUE)
		# result <- rbind(result, fitAverage, fitSE)
		# return(result)
	# }
# })
