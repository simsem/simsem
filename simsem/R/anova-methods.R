# this is based on the anova function in the lmer/lavaan package

setMethod("anova", signature(object = "SimResult"), function(object, ...) {
    
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
    nseed <- mods[[1]]@seed
    for (i in 2:length(mods)) {
        nseed <- c(nseed, mods[[1]]@seed)
    }
    if (any(!duplicated(nseed)[2:length(mods)])) 
        stop("simSEM ERROR: Models are based on different data and cannont be compared, check you random seed")
    
    # put them in order (using number of free parameters) nfreepar <- sapply(lapply(mods, logLik), attr, 'df')
    nfreepar <- mods[[1]]@fit$df[1]
    for (i in 2:length(mods)) {
        nfreepar <- c(nfreepar, mods[[i]]@fit$df[1])
    }
    
    
    if (any(duplicated(nfreepar))) 
        stop("simSEM ERROR: Two models have the same degrees of freedom and cannot be nested")
    ## FIXME: what to do here?
    
    # what, same number of free parameters?
    
    # right now we stop things and give a warning.
    
    # stop('simSEM ERROR: Two models have the same degrees of freedom and cannot be nested')
    
    # ORDERING DOES NOT WORK RIGHT NOW. Why??
    mods <- mods[order(nfreepar, decreasing = FALSE)]
    
	
	if(!multipleAllEqualList(lapply(mods, function(x, name) unique(slot(x, name)), name="n"))) stop("Models are based on different values of sample sizes")
	if(!multipleAllEqualList(lapply(mods, function(x, name) unique(slot(x, name)), name="pmMCAR"))) stop("Models are based on different values of the percent completely missing at random")
	if(!multipleAllEqualList(lapply(mods, function(x, name) unique(slot(x, name)), name="pmMAR"))) stop("Models are based on different values of the percent missing at random")
	
	nrep <- dim(object@fit)[[1]]
	x <- NULL
    pred <- NULL
    
    if (length(object@n) > 1) {
        if (!length(object@n) == nrep) {
            stop("Number of random sample sizes is not the same as the number of replications, check to see if N varied across replications")
        }
        x <- cbind(x, object@n)
        pred$N <- unique(round(seq(min(object@n), max(object@n), length.out=20)))
    }
    if (length(object@pmMCAR) > 1) {
        if (!length(object@pmMCAR) == nrep) {
            stop("Number of random pmMCARs is not the same as the number of replications, check to see if pmMCAR varied across replications")
        }
        x <- cbind(x, object@pmMCAR)
        pred$MCAR <- seq(min(object@pmMCAR), max(object@pmMCAR), length.out=20)
        
    }
    if (length(object@pmMAR) > 1) {
        if (!length(object@pmMAR) == nrep) {
            stop("Number of random pmMARs is not the same as the number of replications, check to see if pmMAR varied across replications")
        }
        x <- cbind(x, object@pmMAR)
        pred$MAR <- seq(min(object@pmMAR), max(object@pmMAR), length.out=20)
        
    }
    # Need to pull fit statistics from each model, compare each one...
    
    # Use apply and diff function to get differneces for each rows
    
    # collect statistics for each model
    Df <- matrix(unlist(lapply(mods, function(x) slot(x, "fit")$df)), ncol = length(mods))
    Chi <- matrix(unlist(lapply(mods, function(x) slot(x, "fit")$Chi)), ncol = length(mods))
    CFI <- matrix(unlist(lapply(mods, function(x) slot(x, "fit")$CFI)), ncol = length(mods))
    TLI <- matrix(unlist(lapply(mods, function(x) slot(x, "fit")$TLI)), ncol = length(mods))
    RMSEA <- matrix(unlist(lapply(mods, function(x) slot(x, "fit")$RMSEA)), ncol = length(mods))
    AIC <- matrix(unlist(lapply(mods, function(x) slot(x, "fit")$AIC)), ncol = length(mods))
    BIC <- matrix(unlist(lapply(mods, function(x) slot(x, "fit")$BIC)), ncol = length(mods))
    
    # difference statistics. Taking the absolute value so order models entered doesn't matter
    Chi.delta <- (apply(Chi, 1, diff))
    Df.delta <- (apply(Df, 1, diff))
    CFI.delta <- (apply(CFI, 1, diff))
    TLI.delta <- (apply(TLI, 1, diff))
    RMSEA.delta <- (apply(RMSEA, 1, diff))
    AIC.delta <- (apply(AIC, 1, diff))
    BIC.delta <- (apply(BIC, 1, diff))
    
    # Power of test. 0 = not siginficant, 1 = sig.
    Power.delta <- pchisq(Chi.delta, Df.delta, lower = FALSE) < 0.05
    
    # Need to think about what we want out of this. Maybe just mean differences across models? Lets do that for now
    val <- data.frame(Df = colMeans(Df), Chisq = colMeans(Chi), CFI = colMeans(CFI), TLI = colMeans(TLI), RMSEA = colMeans(RMSEA), AIC = colMeans(AIC), BIC = colMeans(BIC))
	
	diff <- c(mean(Chi.delta), mean(Df.delta), mean(Power.delta), mean(CFI.delta), mean(TLI.delta), 
        mean(RMSEA.delta), mean(AIC.delta), mean(BIC.delta))
    colnames(val) <- c("df", "chisq", "CFI", "TLI", "RMSEA", "AIC", "BIC")
	names(diff) <- c("Chisq diff", "Df diff", "Power", "CFI diff", "TLI diff", "RMSEA diff", 
        "AIC diff", "BIC diff")

	varyResult <- NULL
		
	if(!is.null(x)) {
	Chi.delta <- as.matrix(Chi.delta)
	Df.delta <- as.matrix(Df.delta)
	temp <- list()	
	# Varying parameters
	ivVal <- expand.grid(pred)
	powVal <- as.list(data.frame(t(ivVal)))
	for(i in 1:ncol(Chi.delta)) {
		temp[[i]] <- sapply(powVal, pValueVariedCutoff, cutoff=rep(qchisq(0.95, df=Df.delta[1, i])), obtainedValue = Chi.delta[,i], revDirec=FALSE, x = x)
	}
	temp <- data.frame(temp)
	varyResult <- data.frame(ivVal, temp)
	colnames(varyResult) <- c(names(pred), paste("power.", 1:ncol(temp), sep=""))
	rownames(varyResult) <- NULL
	} 
	result <- list(summary = val, diff = diff, varyParam = varyResult)
    return(result)
    
	# Return List
	# If n, pmMCAR, pmMAR are varying, return the additional arguments in the list that provide the conditional median of the varying parameter.
})

# this is based on the anova function in the lmer/lavaan package
setMethod("anova", signature(object = "SimModelOut"), function(object, ...) {
    
    mcall <- match.call(expand.dots = TRUE)
    dots <- list(...)
    modp <- if (length(dots)) 
        sapply(dots, is, "SimModelOut") else logical(0)
    
    # single argument version is not supported (what should be display?)
    if (!any(modp)) 
        stop("simSEM ERROR: need two models to compare")
    
    # list of models
    mods <- c(list(object), dots[modp])
    names(mods) <- sapply(as.list(mcall)[c(FALSE, TRUE, modp)], as.character)
    
    # put them in order (using number of free parameters) nfreepar <- sapply(lapply(mods, logLik), attr, 'df')
    nfreepar <- mods[[1]]@fit["df"][1]
    for (i in 2:length(mods)) {
        nfreepar <- c(nfreepar, mods[[i]]@fit["df"][1])
    }
    
    
    # ORDERING DOES NOT WORK RIGHT NOW. Why??
    mods <- mods[order(nfreepar, decreasing = FALSE)]
    
    # Need to pull fit statistics from each model, compare each one...
    
    # Use apply and diff function to get differneces for each rows
    
    # collect statistics for each model
    Df <- matrix(unlist(lapply(mods, function(x) slot(x, "fit")["df"])), ncol = length(mods))
    Chi <- matrix(unlist(lapply(mods, function(x) slot(x, "fit")["Chi"])), ncol = length(mods))
    CFI <- matrix(unlist(lapply(mods, function(x) slot(x, "fit")["CFI"])), ncol = length(mods))
    TLI <- matrix(unlist(lapply(mods, function(x) slot(x, "fit")["TLI"])), ncol = length(mods))
    RMSEA <- matrix(unlist(lapply(mods, function(x) slot(x, "fit")["RMSEA"])), ncol = length(mods))
    AIC <- matrix(unlist(lapply(mods, function(x) slot(x, "fit")["AIC"])), ncol = length(mods))
    BIC <- matrix(unlist(lapply(mods, function(x) slot(x, "fit")["BIC"])), ncol = length(mods))
   
    # difference statistics. Taking the absolute value so order models entered doesn't matter
    Chi.delta <- (apply(Chi, 1, diff))
    Df.delta <- (apply(Df, 1, diff))
    CFI.delta <- (apply(CFI, 1, diff))
    TLI.delta <- (apply(TLI, 1, diff))
    RMSEA.delta <- (apply(RMSEA, 1, diff))
    AIC.delta <- (apply(AIC, 1, diff))
    BIC.delta <- (apply(BIC, 1, diff))
    
    
	pValue <- NULL
    if (!any(duplicated(nfreepar))) pValue <- pchisq(Chi.delta, Df.delta, lower = FALSE)
    
	val <- data.frame(Df = colMeans(Df), Chisq = colMeans(Chi), CFI = colMeans(CFI), TLI = colMeans(TLI), RMSEA = colMeans(RMSEA), AIC = colMeans(AIC), BIC = colMeans(BIC))
	
		
    # Need to think about what we want out of this. Maybe just mean differences across models? Lets do that for now
    diff <- data.frame(Chisq.diff = c(NA, Chi.delta), Df.diff = c(NA, Df.delta), pValue = c(NA, pValue), CFI.diff = c(NA, 
        CFI.delta), TLI.diff = c(NA, TLI.delta), RMSEA.diff = c(NA, RMSEA.delta), AIC.diff = c(NA, AIC.delta), BIC.diff = c(NA, BIC.delta))
    #' Pr(>Chisq)' = Pvalue.delta, Don't report mean p value, meaningless?
    
    result <- list(summary = val, diff = diff)
    return(result)
    
})
 