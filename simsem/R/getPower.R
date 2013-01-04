# getPower: automatically find the power for all values in the range of given
# varying parameters or for a set of given value of varying parameters

getPower <- function(simResult, alpha = 0.05, contParam = NULL, powerParam = NULL, 
    nVal = NULL, pmMCARval = NULL, pmMARval = NULL, paramVal = NULL) {
    object <- simResult$clean()
    condition <- c(length(unique(object$pmMCAR)) > 1, length(unique(object$pmMAR)) > 
        1, length(unique(object$n)) > 1)
    if (any(condition)) {
        pred <- NULL
        pred$N <- nVal
        pred$MCAR <- pmMCARval
        pred$MAR <- pmMARval
        if (!is.null(paramVal)) {
            if (is(paramVal, "list")) {
                if (is.null(names(paramVal))) 
                  names(paramVal) <- contParam
                pred <- c(pred, paramVal)
            } else if (is.vector(paramVal)) {
                if (length(contParam) == 1) {
                  temp <- list(paramVal)
                  names(temp) <- contParam
                  pred <- c(pred, temp)
                } else {
                  temp <- as.list(paramVal)
                  names(temp) <- contParam
                  pred <- c(pred, temp)
                }
            }
        }
        pow <- continuousPower(object, length(unique(object$n)) > 1, length(unique(object$pmMCAR)) > 
            1, length(unique(object$pmMAR)) > 1, contParam = contParam, alpha = alpha, 
            powerParam = powerParam, pred = pred)
        return(pow)
    } else {
        coef <- colMeans(object$coef, na.rm = TRUE)
        real.se <- sapply(object$coef, sd, na.rm = TRUE)
        estimated.se <- colMeans(object$se, na.rm = TRUE)
        estimated.se[estimated.se == 0] <- NA
        z <- object$coef/object$se
        crit.value <- qnorm(1 - alpha/2)
        sig <- abs(z) > crit.value
        pow <- apply(sig, 2, mean, na.rm = TRUE)
        return(pow)
    }
    
}

# continuousPower: Function to calculate power with continously varying
# parameters Will calculate power over continuously varying n, percent missing,
# or parameters

continuousPower <- function(simResult, contN = TRUE, contMCAR = FALSE, contMAR = FALSE, 
    contParam = NULL, alpha = 0.05, powerParam = NULL, pred = NULL) {
    
    # Change warning option to supress warnings
    warnT <- as.numeric(options("warn"))
    options(warn = -1)
    
    # Clean simResult object and get a replications by parameters matrix of 0s and
    # 1s for logistic regression
    object <- simResult$clean()
    crit.value <- qnorm(1 - alpha/2)
    sig <- 0 + (abs(object$coef/object$se) > crit.value)
    nrep <- dim(sig)[[1]]
    
    # Find paramaterss to get power for
    if (!is.null(powerParam)) {
        j <- match(powerParam, dimnames(sig)[[2]])  # Return column indices that start with 'param'
        sig <- data.frame(sig[, j])
    }
    
    
    
    # Create matrix of predictors (randomly varying params)
    x <- NULL
    predDefault <- is.null(pred)
    
    if (contN) {
        if (!length(object$n) == nrep) {
            stop("Number of random sample sizes is not the same as the number of replications, check to see if N varied across replications")
        }
        x <- cbind(x, object$n)
        if (predDefault) 
            pred$N <- min(object$n):max(object$n)
    }
    if (contMCAR) {
        if (!length(object$pmMCAR) == nrep) {
            stop("Number of random pmMCARs is not the same as the number of replications, check to see if pmMCAR varied across replications")
        }
        x <- cbind(x, object$pmMCAR)
        if (predDefault) 
            pred$MCAR <- seq(min(object$pmMCAR), max(object$pmMCAR), by = 0.01)
        
    }
    if (contMAR) {
        if (!length(object$pmMAR) == nrep) {
            stop("Number of random pmMARs is not the same as the number of replications, check to see if pmMAR varied across replications")
        }
        x <- cbind(x, object$pmMAR)
        if (predDefault) 
            pred$MAR <- seq(min(object$pmMAR), max(object$pmMAR), by = 0.01)
        
    }
    if (!is.null(contParam)) {
        if (!(dim(object$paramValue)[[1]] == nrep)) {
            stop("Number of random parameters is not the same as the number of replications, check to see if parameters varied across replications")
        }
        j <- match(contParam, names(object$paramValue))  # Return column indices that start with 'contParam'
        x <- cbind(x, object$paramValue[, j])
        if (predDefault) {
            paramVal <- list()
            for (i in 1:length(contParam)) {
                temp <- seq(min(object$paramValue[, contParam[i]]), max(object$paramValue[, 
                  contParam[i]]), length.out = 5)
                paramVal[[i]] <- unique(temp)
            }
            names(paramVal) <- contParam
            pred <- c(pred, paramVal)
        }
    }
    
    res <- NULL
    powVal <- data.frame(expand.grid(pred))
    powVal <- cbind(rep(1, dim(powVal)[1]), powVal)
    x <- as.matrix(x)
    for (i in 1:dim(sig)[[2]]) {
        mod <- invisible(try(glm(sig[, i] ~ x, family = binomial(link = "logit")), 
            silent = TRUE))
        res[[dimnames(sig)[[2]][[i]]]] <- apply(powVal, 1, predProb, mod)
    }
    if (is.list(res)) {
        res <- do.call(cbind, res)
    } else {
        res <- t(as.matrix(res))
    }
    names(res) <- names(sig)
    colnames(powVal) <- paste("iv.", colnames(powVal), sep = "")
    pow <- cbind(powVal[, -1], res)
    colnames(pow) <- c(colnames(powVal)[-1], colnames(res))
    
    
    ## Return warnings setting to user's settings
    options(warn = warnT)
    
    return(pow)
}

## predProb: Function to get predicted probabilities from logistic regression

# \title{
	# Function to get predicted probabilities from logistic regression
# }
# \description{
	# Function to get predicted probabilities from logistic regression
# }
# \usage{
# predProb(newdat, glmObj)
# }
# \arguments{
# \item{newdat}{
	# A vector of values for all predictors, including the intercept
# }
  # \item{glmObj}{
	# An object from a fitted glm run with a logit link
# }
# }
# \value{
	# Predictive probability of success given the values in the \code{newdat} argument.
# }

predProb <- function(newdat, glmObj) {
    slps <- as.numeric(coef(glmObj))
    logi <- sum(newdat * slps)
    pp <- exp(logi)/(1 + exp(logi))
    return(pp)
}
 
