# plotPower: plot the power curve given one or two varying parameters

plotPower <- function(object, powerParam, alpha = 0.05, contParam = NULL, contN = TRUE, 
    contMCAR = TRUE, contMAR = TRUE, useContour = TRUE) {
    object <- clean(object)
    
    crit.value <- qnorm(1 - alpha/2)
    sig <- 0 + (abs(object@coef/object@se) > crit.value)
    colnames(sig) <- colnames(object@coef)
    nrep <- dim(sig)[[1]]
    if (is.null(powerParam)) 
        stop("Please specify the parameter used to plot")
    j <- match(powerParam, dimnames(sig)[[2]])  # Return column indices that start with 'param'
	if(length(j) == 0) stop("The specified parameter does not match with any parameter names in the object.")
    sig <- as.matrix(sig[, j])
    
    # Create matrix of predictors (randomly varying params)
    x <- NULL
    pred <- NULL
    
    if ((length(unique(object@n)) > 1) && contN) {
        if (!length(object@n) == nrep) {
            stop("Number of random sample sizes is not the same as the number of replications, check to see if N varied across replications")
        }
        x <- cbind(x, object@n)
        pred$N <- min(object@n):max(object@n)
    }
    if ((length(unique(object@pmMCAR)) > 1) && contMCAR) {
        if (!length(object@pmMCAR) == nrep) {
            stop("Number of random pmMCARs is not the same as the number of replications, check to see if pmMCAR varied across replications")
        }
        x <- cbind(x, object@pmMCAR)
        pred$MCAR <- seq(min(object@pmMCAR), max(object@pmMCAR), by = 0.01)
        
    }
    if ((length(unique(object@pmMAR)) > 1) && contMAR) {
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
            temp <- seq(min(object@paramValue[, contParam[i]]), max(object@paramValue[, 
                contParam[i]]), length.out = 50)
            paramVal[[i]] <- unique(temp)
        }
        names(paramVal) <- contParam
        pred <- c(pred, paramVal)
    }
    plotPowerSig(sig, x, xval = pred, mainName = powerParam, useContour = useContour)
    
}

# plotPowerSig: plot the power curve given one or two varying parameters when a
# data frame of significance or not is specified

# \title{
# Plot multiple logistic curves given a significance result matrix
# }
# \description{
# This function will plot the significance results given the value of predictors. 
# }
# \usage{
# plotPowerSig(sig, x = NULL, xval=NULL, mainName = NULL, useContour = TRUE)
# }
# \arguments{
  # \item{sig}{
	# The \code{data.frame} of a significance result, which contains only \code{TRUE} for significance and \code{FALSE} for not significance.
# }
# \item{x}{
	# The \code{data.frame} of the predictor values. The number of rows of the \code{x} argument should be equal to the number of rows in the \code{object}.
# }
# \item{xval}{
	# The values of predictor that researchers would like to find the fit indices cutoffs from.
# }
  # \item{mainName}{
	# A vector of the titles of the graphs
# }
  # \item{useContour}{
	# If there are two of sample size, percent completely at random, and percent missing at random are varying, the \code{plotCutoff} function will provide 3D graph. Contour graph is a default. However, if this is specified as \code{FALSE}, perspective plot is used.
# }
# }
# \value{
	# NONE. Only plot the fit indices distributions.
# }

plotPowerSig <- function(sig, x = NULL, xval = NULL, mainName = NULL, useContour = TRUE) {
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
        mod <- invisible(try(glm(sig[, i] ~ x, family = binomial(link = "logit")), 
            silent = TRUE))
        if (ncol(x) == 1) {
            predVal <- apply(data.frame(1, xval), 1, predProb, mod)
            plot(xval[[1]], predVal, type = "n", xlab = names(xval)[1], ylab = "Power", 
                main = mainName[i], ylim = c(0, 1))
            lines(xval[[1]], predVal)
        } else if (ncol(x) == 2) {
            FUN <- function(x, y) {
                logi <- mod$coefficients[1] + mod$coefficients[2] * x + mod$coefficients[3] * 
                  y
                pp <- exp(logi)/(1 + exp(logi))
                return(pp)
            }
            zpred <- outer(xval[[1]], xval[[2]], FUN)
            if (useContour) {
                contour(xval[[1]], xval[[2]], zpred, xlab = names(xval)[1], ylab = names(xval)[2], 
                  main = mainName[i])
            } else {
                persp(xval[[1]], xval[[2]], zpred, zlim = c(0, 1), theta = 30, phi = 30, 
                  expand = 0.5, col = "lightblue", ltheta = 120, shade = 0.75, ticktype = "detailed", 
                  xlab = names(xval)[1], ylab = names(xval)[2], main = mainName[i], 
                  zlab = "Power")
            }
        } else {
            stop("Something is wrong!")
        }
    }
    if (ncol(sig) > 1) 
        par(obj)
    options(warn = warnT)
} 
