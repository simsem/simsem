# plotPower: plot the power curve given one or two varying parameters

plotPower <- function(object, powerParam, alpha = 0.05, contParam = NULL, contN = TRUE, contMCAR = TRUE, contMAR = TRUE, useContour = TRUE) {
    object <- clean(object)
    
    crit.value <- qnorm(1 - alpha/2)
    sig <- 0 + (abs(object@coef/object@se) > crit.value)
    colnames(sig) <- colnames(object@coef)
    nrep <- dim(sig)[[1]]
    if (is.null(powerParam)) 
        stop("Please specify the parameter used to plot")
    j <- match(powerParam, dimnames(sig)[[2]])  # Return column indices that start with 'param'
    sig <- as.matrix(sig[, j])
    
    # Create matrix of predictors (randomly varying params)
    x <- NULL
    pred <- NULL
    
    if ((length(object@n) > 1) && contN) {
        if (!length(object@n) == nrep) {
            stop("Number of random sample sizes is not the same as the number of replications, check to see if N varied across replications")
        }
        x <- cbind(x, object@n)
        pred$N <- min(object@n):max(object@n)
    }
    if ((length(object@pmMCAR) > 1) && contMCAR) {
        if (!length(object@pmMCAR) == nrep) {
            stop("Number of random pmMCARs is not the same as the number of replications, check to see if pmMCAR varied across replications")
        }
        x <- cbind(x, object@pmMCAR)
        pred$MCAR <- seq(min(object@pmMCAR), max(object@pmMCAR), by = 0.01)
        
    }
    if ((length(object@pmMAR) > 1) && contMAR) {
        if (!length(object@pmMAR) == nrep) {
            stop("Number of random pmMARs is not the same as the number of replications, check to see if pmMAR varied across replications")
        }
        x <- cbind(x, object@pmMAR)
        pred$MAR <- seq(min(object@pmMAR), max(object@pmMAR), by = 0.01)
        
    }
    if (!is.null(contParam)) {
        if (!(dim(object@paramValue)[[1]] == nrep)) {
            stop("Number of random parameters is not the same as the number of replications, check to see if parameters varied across replications")
        }
        j <- match(contParam, names(object@paramValue))  # Return column indices that start with 'contParam'
        x <- cbind(x, object@paramValue[, j])
        paramVal <- list()
        for (i in 1:length(contParam)) {
            temp <- seq(min(object@paramValue[, contParam[i]]), max(object@paramValue[, contParam[i]]), length.out = 50)
            paramVal[[i]] <- unique(temp)
        }
        names(paramVal) <- contParam
        pred <- c(pred, paramVal)
    }
	plotPowerSig(sig, x, powerParam, useContour=useContour)
    
} 

plotPowerSig <- function(sig, x = NULL, mainName = NULL, useContour = TRUE) {
    warnT <- as.numeric(options("warn"))
    options(warn = -1)
    if (is.null(x)) 
        stop("There is no varying parameter in this object.")
    if (ncol(x) > 2) 
        stop("The number of independent variables cannot be over 2. Please reduce 'contParam' or specify some of 'contN', 'contMCAR', and 'contMAR' as FALSE.")
    if (ncol(sig) == 2) {
        obj <- par(mfrow = c(1, 2))
    } else if (ncol(sig) == 3) {
        obj <- par(mfrow = c(1, 3))
    } else if (ncol(sig) > 3) {
        obj <- par(mfrow = c(2, ceiling(ncol(sig)/2)))
    } else if (ncol(sig) == 1) {
        # Intentionally leaving as blank
    } else {
        stop("Some errors occur")
    }
    
    for (i in 1:ncol(sig)) {
        mod <- invisible(try(glm(sig[, i] ~ x, family = binomial(link = "logit")), silent = TRUE))
        if (ncol(x) == 1) {
            predVal <- apply(data.frame(1, pred), 1, predProb, mod)
            plot(pred[[1]], predVal, type = "n", xlab = names(pred)[1], ylab = "Power", main = mainName[i], ylim = c(0, 1))
            lines(pred[[1]], predVal)
        } else if (ncol(x) == 2) {
            FUN <- function(x, y) {
                logi <- mod$coefficients[1] + mod$coefficients[2] * x + mod$coefficients[3] * y
                pp <- exp(logi)/(1 + exp(logi))
                return(pp)
            }
            zpred <- outer(pred[[1]], pred[[2]], FUN)
            if (useContour) {
                contour(pred[[1]], pred[[2]], zpred, xlab = names(pred)[1], ylab = names(pred)[2], main = mainName[i])
            } else {
                persp(pred[[1]], pred[[2]], zpred, zlim = c(0, 1), theta = 30, phi = 30, expand = 0.5, col = "lightblue", ltheta = 120, 
                  shade = 0.75, ticktype = "detailed", xlab = names(pred)[1], ylab = names(pred)[2], main = mainName[i], zlab = "Power")
            }
        } else {
            stop("Something is wrong!")
        }
    }
    if (ncol(sig) > 1) 
        par(obj)
    options(warn = warnT)
}