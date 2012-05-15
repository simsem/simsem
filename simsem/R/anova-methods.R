# this is based on the anova function in the lmer/lavaan package
setMethod("anova", signature(object = "SimResult"), function(object, ...) {
    
    mcall <- match.call(expand.dots = TRUE)
    dots <- list(...)
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
    val <- data.frame(Df = colMeans(Df), Chisq = colMeans(Chi), CFI = colMeans(CFI), TLI = colMeans(TLI), RMSEA = colMeans(RMSEA), AIC = colMeans(AIC), BIC = colMeans(BIC), c(NA, mean(Chi.delta)), 
        c(NA, mean(Df.delta)), c(NA, mean(Power.delta)), c(NA, mean(CFI.delta)), c(NA, mean(TLI.delta)), c(NA, mean(RMSEA.delta)), c(NA, mean(AIC.delta)), c(NA, mean(BIC.delta)))
    colnames(val) <- c("df", "chisq", "CFI", "TLI", "RMSEA", "AIC", "BIC", "Chisq diff", "Df diff", "Power", "CFI diff", "TLI diff", "RMSEA diff", "AIC diff", "BIC diff")
    class(val) <- c("anova", class(val))
    return(val)
    
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
    
    # Need to pull fit statistics from each model, compare each one...
    
    # Use apply and diff function to get differneces for each rows
    
    # collect statistics for each model
    Df <- matrix(unlist(lapply(mods, function(x) slot(x, "fit")$df)), ncol = length(mods))
    Chi <- matrix(unlist(lapply(mods, function(x) slot(x, "fit")$Chi)), ncol = length(mods))
    CFI <- matrix(unlist(lapply(mods, function(x) slot(x, "fit")$CFI)), ncol = length(mods))
    TLI <- matrix(unlist(lapply(mods, function(x) slot(x, "fit")$TLI)), ncol = length(mods))
    RMSEA <- matrix(unlist(lapply(mods, function(x) slot(x, "fit")$RMSEA)), ncol = length(mods))
    
    
    # difference statistics. Taking the absolute value so order models entered doesn't matter
    Chi.delta <- (apply(Chi, 1, diff))
    Df.delta <- (apply(Df, 1, diff))
    CFI.delta <- (apply(CFI, 1, diff))
    TLI.delta <- (apply(TLI, 1, diff))
    RMSEA.delta <- (apply(RMSEA, 1, diff))
    
    # Power of test. 0 = not siginficant, 1 = sig.
    Power.delta <- pchisq(Chi.delta, Df.delta, lower = FALSE) < 0.05
    
    # Need to think about what we want out of this. Maybe just mean differences across models? Lets do that for now
    val <- data.frame(Chisq.diff = c(NA, mean(Chi.delta)), Df.diff = c(NA, mean(Df.delta)), Power = c(NA, mean(Power.delta)), CFI.diff = c(NA, mean(CFI.delta)), TLI.diff = c(NA, mean(TLI.delta)), 
        RMSEA.diff = c(NA, mean(RMSEA.delta)))
    #' Pr(>Chisq)' = Pvalue.delta, Don't report mean p value, meaningless?
    
    
    class(val) <- c("anova", class(val))
    
    return(val)
    
})
 
