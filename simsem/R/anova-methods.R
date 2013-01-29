# this is based on the anova function in the lmer/lavaan package

setMethod("anova", signature(object = "SimResult"), function(object, ..., usedFit = NULL) {
    usedFit <- cleanUsedFit(usedFit, colnames(object@fit))
	if("df" %in% colnames(object@fit) & "chisq" %in% colnames(object@fit)) {
		usedFit <- c("chisq", "df", setdiff(usedFit, c("chisq", "df")))
	}
    mcall <- match.call(expand.dots = TRUE)
    mod <- clean(object, ...)
    object <- mod[[1]]
    dots <- mod[2:length(mod)]
    modp <- if (length(dots)) 
        sapply(dots, is, "SimResult") else logical(0)
    
    # single argument version is not supported (what should be display?)
    if (!any(modp)) 
        stop("simSEM ERROR: need two models to compare")
    
    # list of models
    mods <- c(list(object), dots[modp])
    names(mods) <- sapply(as.list(mcall)[c(FALSE, TRUE, modp)], as.character)
    
    # Make sure models come from the same seed else stop and give warning
    nseed <- mods[[1]]@seed[1]
    for (i in 2:length(mods)) {
        nseed <- rbind(nseed, mods[[1]]@seed[1])
    }
    if (any(!duplicated(nseed)[2:length(mods)])) 
        stop("simSEM ERROR: Models are based on different data and cannont be compared, check you random seed")
    
    # put them in order (using number of free parameters) nfreepar <-
    # sapply(lapply(mods, logLik), attr, 'df')
    nfreepar <- mods[[1]]@fit$df[1]
	if(!is.null(nfreepar)) {
		for (i in 2:length(mods)) {
			nfreepar <- c(nfreepar, mods[[i]]@fit$df[1])
		}
		
		
		if (any(duplicated(nfreepar))) 
			stop("simSEM ERROR: Two models have the same degrees of freedom and cannot be nested")
		## FIXME: what to do here?
		
		# what, same number of free parameters?
		
		# right now we stop things and give a warning.
		
		# stop('simSEM ERROR: Two models have the same degrees of freedom and cannot be
		# nested')
		
		# ORDERING DOES NOT WORK RIGHT NOW. Why??
		mods <- mods[order(nfreepar, decreasing = FALSE)]
    }
    
    if (!multipleAllEqualList(lapply(mods, function(x, name) unique(slot(x, name)), 
        name = "n"))) 
        stop("Models are based on different values of sample sizes")
    if (!multipleAllEqualList(lapply(mods, function(x, name) unique(slot(x, name)), 
        name = "pmMCAR"))) 
        stop("Models are based on different values of the percent completely missing at random")
    if (!multipleAllEqualList(lapply(mods, function(x, name) unique(slot(x, name)), 
        name = "pmMAR"))) 
        stop("Models are based on different values of the percent missing at random")
    
    nrep <- dim(object@fit)[[1]]
    x <- NULL
    pred <- NULL
    
    if (length(unique(object@n)) > 1) {
        if (!length(object@n) == nrep) {
            stop("Number of random sample sizes is not the same as the number of replications, check to see if N varied across replications")
        }
        x <- cbind(x, object@n)
        pred$N <- unique(round(seq(min(object@n), max(object@n), length.out = 20)))
    }
    if (length(unique(object@pmMCAR)) > 1) {
        if (!length(object@pmMCAR) == nrep) {
            stop("Number of random pmMCARs is not the same as the number of replications, check to see if pmMCAR varied across replications")
        }
        x <- cbind(x, object@pmMCAR)
        pred$MCAR <- seq(min(object@pmMCAR), max(object@pmMCAR), length.out = 20)
        
    }
    if (length(unique(object@pmMAR)) > 1) {
        if (!length(object@pmMAR) == nrep) {
            stop("Number of random pmMARs is not the same as the number of replications, check to see if pmMAR varied across replications")
        }
        x <- cbind(x, object@pmMAR)
        pred$MAR <- seq(min(object@pmMAR), max(object@pmMAR), length.out = 20)
        
    }
    # Need to pull fit statistics from each model, compare each one...
    
    # Use apply and diff function to get differneces for each rows
    
    # collect statistics for each model
	modsout <- lapply(mods, function(x) slot(x, "fit")[,usedFit])
	mat <- list()
	for(i in seq_along(usedFit)) {
		mat[[i]] <- sapply(modsout, function(x) x[,usedFit[i]])
	}
	names(mat) <- usedFit
	matDelta <- lapply(mat, function(x) apply(x, 1, diff))

	val <- sapply(mat, colMeans)
	rownames(val) <- paste("Object", 1:nrow(val))
	diff <- sapply(matDelta, mean)
	names(diff) <- paste(names(diff), "diff")
	
    # Power of test. 0 = not siginficant, 1 = sig.
	if("chisq" %in% names(mat) & "df" %in% names(mat)) {
		Chi.delta <- matDelta$chisq
		Df.delta <- matDelta$df
		diff <- diff[!(names(mat) %in% c("chisq", "df"))]
		Power.delta <- pchisq(Chi.delta, Df.delta, lower.tail = FALSE) < 0.05
		diff <- c("chisq diff" = mean(Chi.delta), "df diff" = mean(Df.delta), "power diff" = mean(Power.delta), diff)
    }

    varyResult <- NULL
    
	
    if (!is.null(x) && ("chisq" %in% names(mat) & "df" %in% names(mat))) {
        Chi.delta <- as.matrix(matDelta$chisq)
        Df.delta <- as.matrix(matDelta$df)
        temp <- list()
        # Varying parameters
        ivVal <- expand.grid(pred)
        powVal <- as.list(data.frame(t(ivVal)))
        for (i in 1:ncol(Chi.delta)) {
            temp[[i]] <- sapply(powVal, pValueVariedCutoff, cutoff = rep(qchisq(0.95, 
                df = Df.delta[1, i])), obtainedValue = Chi.delta[, i], revDirec = FALSE, 
                x = x)
        }
        temp <- data.frame(temp)
        varyResult <- data.frame(ivVal, temp)
        colnames(varyResult) <- c(names(pred), paste("power.", 1:ncol(temp), sep = ""))
        rownames(varyResult) <- NULL
    }
    result <- list(summary = val, diff = diff, varyParam = varyResult)
    return(result)
    
    # Return List If n, pmMCAR, pmMAR are varying, return the additional arguments
    # in the list that provide the conditional median of the varying parameter.
}) 
