# Function to calculate power with continously varying parametersWill calculate
# power over continuously varying n, percent missing, or parameters

## Doesn't work when only 1 parameter is specified. May need an if statement...

continuousPower <- function(simResult, contN = TRUE, contMCAR = FALSE, 
    contMAR = FALSE, contParam = NULL, alpha = 0.05, powerParam = NULL) {
    
    # Change warning option to supress warnings
    warnT <- as.numeric(options("warn"))
    options(warn = -1)
    
    # Clean simResult object and get a replications X parameters matrix of 0s and
    # 1s for logistic regression
    object <- clean(simResult)
    crit.value <- qnorm(1 - alpha/2)
    sig <- 0 + (abs(object@coef/object@se) > crit.value)
    nrep <- dim(sig)[[1]]
    
    ## Find params to get power for
    if (!is.null(powerParam)) {
        j <- grep(powerParam, dimnames(sig)[[2]])  # Return column indices that start with 'param'
        sig <- data.frame(sig[, j])
    }
    
    
    
    # Create matrix of predictors (randomly varying params)
    x <- NULL
    pred <- NULL
    
    if (contN) {
        if (!length(object@n) == nrep) {
            stop("Number of random sample sizes is not the same as the number of replications, check to see if N varied across replications")
        }
        x <- cbind(x, object@n)
        pred$N <- min(object@n):max(object@n)
    }
    if (contMCAR) {
        if (!length(object@pmMCAR) == nrep) {
            stop("Number of random pmMCARs is not the same as the number of replications, check to see if pmMCAR varied across replications")
        }
        x <- cbind(x, object@pmMCAR)
        pred$MCAR <- seq(min(object@pmMCAR), max(object@pmMCAR), by = 0.01)
        
    }
    if (contMAR) {
        if (!length(object@pmMAR) == nrep) {
            stop("Number of random pmMARs is not the same as the number of replications, check to see if pmMAR varied across replications")
        }
        x <- cbind(x, object@pmMAR)
        pred$MAR <- seq(min(object@pmMAR), max(object@pmMAR), by = 0.01)
        
    }
    if (!is.null(contParam)) {
        if (!(dim(object@paramValue)[[2]] == nrep)) {
            stop("Number of random parameters is not the same as the number of replications, check to see if parameters varied across replications")
        }
        j <- grep(contParam, names(object@paramValue))  # Return column indices that start with 'contParam'
        x <- cbind(x, object@paramValue[, j])
        # need way to get predicted values in here...
    }
    
    res <- NULL
    powVal <- data.frame(expand.grid(pred))
    # need to put a column of 1s in front of powVal
    powVal <- cbind(rep(1, dim(powVal)[1]), powVal)
    
    # Need way to handle params when power is 1 or 0... or atleast suppress
    # warnings WHY can't I predict more values than nRep!!!!!! F it. Lets write out
    # own predicted probaility function.
    
    # mod<-glm.fit(y =sig[,i], x=x, family=binomial(link = 'logit'))
    
    
    for (i in 1:dim(sig)[[2]]) {
        mod <- invisible(try(glm(sig[, i] ~ x, family = binomial(link = "logit")), 
            silent = TRUE))
        res[[dimnames(sig)[[2]][[i]]]] <- apply(powVal, 1, predProb, mod)
    }
    res <- do.call(cbind, res)
    names(res) <- names(sig)
    pow <- cbind(powVal[, -1], res)
    
    # pow[pow < desiredPower] <- NA #puts NA in for below power deisred
    
    # powCond <- NULL
    
    # Find first column with power > desired power. Only works for 1 random
    # parameters
    
    # if(dim(x)[[2]] == 1) {
    
    # powCond <- as.numeric((lapply(apply(pow > desiredPower, 2, which), head, 1)))
    
    # powCond <- x[powCond][-1]
    
    # names(powCond) <- names(object@coef)
    
    # }
    
    ## Return warnings setting to user's settings
    options(warn = warnT)
    
    ## Currently returns power for all combinations of random parameters for all
    ## model parameters need to make this better.
    return(pow)
}

## predProb: Function to get predicted probabilities from logistic regression

## arguments: newdata (a vector of values for all predictors, including the
## intercept)

## glmobj an object from a fitted glm run with a logit link


predProb <- function(newdat, glmObj) {
    slps <- as.numeric(coef(glmObj))
    logi <- sum(newdat * slps)
    pp <- exp(logi)/(1 + exp(logi))
    return(pp)
} 
