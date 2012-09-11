# A collection of summary*, get*, and set* functions for the result object


# summaryParam: This function will summarize the obtained parameter estimates
# and standard error.

summaryParam <- function(object, alpha = 0.05, detail = FALSE) {
    object <- clean(object)
    coef <- colMeans(object@coef, na.rm = TRUE)
    real.se <- sapply(object@coef, sd, na.rm = TRUE)
    estimated.se <- colMeans(object@se, na.rm = TRUE)
    estimated.se[estimated.se == 0] <- NA
    z <- object@coef/object@se
    crit.value <- qnorm(1 - alpha/2)
    sig <- abs(z) > crit.value
    pow <- apply(sig, 2, mean, na.rm = TRUE)
    stdCoef <- colMeans(object@stdCoef, na.rm = TRUE)
    stdRealSE <- sapply(object@stdCoef, sd, na.rm = TRUE)
    result <- cbind(coef, real.se, estimated.se, pow, stdCoef, stdRealSE)
    colnames(result) <- c("Estimate Average", "Estimate SD", "Average SE", "Power (Not equal 0)", 
        "Std Est", "Std Est SD")
    if (!is.null(object@paramValue)) {
        targetVar <- match(colnames(object@coef), colnames(object@paramValue))
        targetVar <- targetVar[!is.na(targetVar)]
        paramValue <- object@paramValue[, targetVar]
        
        if ((ncol(object@coef) == ncol(paramValue)) && all(colnames(object@coef) == 
            colnames(paramValue))) {
            nRep <- object@nRep
            nParam <- ncol(object@coef)
            if (nrow(object@paramValue) == 1) 
                paramValue <- matrix(unlist(rep(paramValue, nRep)), nRep, nParam, 
                  byrow = T)
            biasParam <- object@coef - paramValue
            crit <- qnorm(1 - alpha/2)
            lowerBound <- object@coef - crit * object@se
            upperBound <- object@coef + crit * object@se
            cover <- (paramValue > lowerBound) & (paramValue < upperBound)
            
            average.param <- apply(paramValue, 2, mean, na.rm = TRUE)
            sd.param <- apply(paramValue, 2, sd, na.rm = TRUE)
            average.bias <- apply(biasParam, 2, mean, na.rm = TRUE)
            perc.cover <- apply(cover, 2, mean, na.rm = TRUE)
            sd.bias <- apply(biasParam, 2, sd, na.rm = TRUE)
            perc.cover[estimated.se == 0] <- NA
            result2 <- cbind(average.param, sd.param, average.bias, sd.bias, perc.cover)
            colnames(result2) <- c("Average Param", "SD Param", "Average Bias", "SD Bias", 
                "Coverage")
            if (nrow(object@paramValue) == 1) 
                result2 <- result2[, c(1, 3, 5)]
            result <- data.frame(result, result2)
            if (detail) {
                relative.bias <- biasParam/paramValue
                relBias <- apply(relative.bias, 2, mean, na.rm = TRUE)
                relBias[is.nan(relBias)] <- NA
                std.bias <- NULL
                relative.bias.se <- NULL
                if (nrow(object@paramValue) == 1) {
                  std.bias <- average.bias/real.se
                  relative.bias.se <- (estimated.se - real.se)/real.se
                } else {
                  std.bias <- average.bias/sd.bias
                  relative.bias.se <- (estimated.se - sd.bias)/sd.bias
                }
                result3 <- cbind(relBias, std.bias, relative.bias.se)
                colnames(result3) <- c("Rel Bias", "Std Bias", "Rel SE Bias")
                result <- data.frame(result, result3)
            }
        }
    }
    if (length(object@FMI1) != 0 & length(object@FMI2) != 0) {
        nRep <- nrow(object@coef)
        nFMI1 <- ncol(object@FMI1)
        FMI1 <- object@FMI1
        FMI2 <- object@FMI2
        average.FMI1 <- apply(FMI1, 2, mean, na.rm = TRUE)
        sd.FMI1 <- apply(FMI1, 2, sd, na.rm = TRUE)
        average.FMI2 <- apply(FMI2, 2, mean, na.rm = TRUE)
        sd.FMI2 <- apply(FMI2, 2, sd, na.rm = TRUE)
        
        resultFMI <- cbind(average.FMI1, sd.FMI1, average.FMI2, sd.FMI2)
        colnames(resultFMI) <- c("Average FMI1", "SD FMI1", "Average FMI2", "SD FMI2")

		targetParam <- matchLavaanName(rownames(result), rownames(resultFMI))
		targetParam <- targetParam[!is.na(targetParam)]

        result <- data.frame(result, resultFMI[targetParam,])
    }
    if (length(unique(object@n)) > 1) {
        corCoefN <- cor(cbind(object@coef, object@n), use = "pairwise.complete.obs")[colnames(object@coef), 
            "object@n"]
        corSeN <- cor(cbind(object@se, object@n), use = "pairwise.complete.obs")[colnames(object@se), 
            "object@n"]
        result <- data.frame(result, r_coef.n = corCoefN, r_se.n = corSeN)
    }
    if (length(unique(object@pmMCAR)) > 1) {
        corCoefMCAR <- cor(cbind(object@coef, object@pmMCAR), use = "pairwise.complete.obs")[colnames(object@coef), 
            "object@pmMCAR"]
        corSeMCAR <- cor(cbind(object@se, object@pmMCAR), use = "pairwise.complete.obs")[colnames(object@se), 
            "object@pmMCAR"]
        result <- data.frame(result, r_coef.pmMCAR = corCoefMCAR, r_se.pmMCAR = corSeMCAR)
    }
    if (length(unique(object@pmMAR)) > 1) {
        corCoefMAR <- cor(cbind(object@coef, object@pmMAR), use = "pairwise.complete.obs")[colnames(object@coef), 
            "object@pmMAR"]
        corSeMAR <- cor(cbind(object@se, object@pmMAR), use = "pairwise.complete.obs")[colnames(object@se), 
            "object@pmMAR"]
        result <- data.frame(result, r_coef.pmMAR = corCoefMAR, r_se.pmMAR = corSeMAR)
    }
    return(as.data.frame(result))
} 

matchLavaanName <- function(name1, name2) {
	result1 <- match(name1, name2)
	result2 <- match(name1, switchLavaanName(name2))
	result1[is.na(result1)] <- result2[is.na(result1)]
	result1
}

switchLavaanName <- function(name) {
	nameX <- strsplit(name, "\\.")
	nameGroup <- sapply(nameX, "[", 1)
	nameParam <- sapply(nameX, "[", 2)
	target <- grep("~~", nameParam)
	varTarget <- strsplit(nameParam[target], "~~")
	nameParam[target] <- paste0(sapply(varTarget, "[", 2), "~~", sapply(varTarget, "[", 1))
	nameX <- paste0(nameGroup, ".", nameParam)
	nameX
}

# summaryMisspec: This function will summarize the obtained fit indices and
# generate a data frame.

summaryMisspec <- function(object) {
    object <- clean(object)
    if (all(dim(object@misspecValue) == 0)) {
        stop("This object does not have any model misspecification.")
    } else {
        misspecAverage <- colMeans(object@misspecValue, na.rm = TRUE)
        misspecSE <- sapply(object@misspecValue, sd, na.rm = TRUE)
        popAverage <- colMeans(object@popFit, na.rm = TRUE)
        popSE <- sapply(object@popFit, sd, na.rm = TRUE)
        mis <- data.frame(mean = misspecAverage, sd = misspecSE)
        pop <- data.frame(mean = popAverage, sd = popSE)
        return(rbind(pop, mis))
    }
} 

# summaryPopulation: Summarize population values behind data generation model

summaryPopulation <- function(object) {
    object <- clean(object)
    paramValue <- object@paramValue
    nRep <- nrow(paramValue)
    nParam <- ncol(paramValue)
    result <- NULL
    if (nrow(object@paramValue) == 1) {
        result <- matrix(paramValue, nrow = 1)
        rownames(result) <- "Population Value"
    } else {
        average.param <- apply(paramValue, 2, mean, na.rm = TRUE)
        sd.param <- apply(paramValue, 2, sd, na.rm = TRUE)
        result <- rbind(average.param, sd.param)
        rownames(result) <- c("Average", "SD")
    }
    return(result)
} 

# summaryFit: This function will summarize the obtained fit indices and
# generate a data frame.

summaryFit <- function(object, alpha = NULL) {
    cleanObj <- clean(object)
    usedFit <- getKeywords()$usedFit
    
    condition <- c(length(unique(object@pmMCAR)) > 1, length(unique(object@pmMAR)) > 
        1, length(unique(object@n)) > 1)
    if (any(condition)) {
        if (is.null(alpha)) 
            alpha <- 0.05
        values <- list()
        ifelse(condition[3], values[[3]] <- round(seq(min(object@n), max(object@n), 
            length.out = 5)), values[[3]] <- NA)
        ifelse(condition[1], values[[1]] <- seq(min(object@pmMCAR), max(object@pmMCAR), 
            length.out = 5), values[[1]] <- NA)
        ifelse(condition[2], values[[2]] <- seq(min(object@pmMAR), max(object@pmMAR), 
            length.out = 5), values[[2]] <- NA)
        m <- do.call(expand.grid, values)
        FUN <- function(vec, obj, alpha, usedFit) getCutoff(obj, alpha, revDirec = FALSE, 
            usedFit = usedFit, nVal = vec[3], pmMCARval = vec[1], pmMARval = vec[2])
        cutoffs <- sapply(as.data.frame(t(m)), FUN, obj = object, alpha = alpha, 
            usedFit = usedFit)
        mSelect <- as.matrix(m[, condition])
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
            # rownames(cutoffs) <- usedFit
        }
        
        fit <- as.data.frame(cleanObj@fit[, usedFit])
        meanfit <- apply(fit, 2, mean, na.rm = TRUE)
        sdfit <- apply(fit, 2, sd, na.rm = TRUE)
		if(length(alpha) == 1) cutoffs <- t(cutoffs)
        result <- cbind(cutoffs, meanfit, sdfit)
        colnames(result) <- c(alpha, "Mean", "SD")
        rownames(result) <- usedFit
        names(dimnames(result)) <- c("Fit Indices", "Alpha")
        # print(as.data.frame(cutoffs))
    }
    
    return(result)
} 

# summaryMisspec: This function will summarize the obtained fit indices and
# generate a data frame.

summaryConverge <- function(object) {
    result <- list()
    converged <- object@converged
    numnonconverged <- sum(!converged)
    if (numnonconverged == 0) 
        stop("You are good! All replications were converged!")
    result <- c(result, list(Converged = c(num.converged = sum(converged), num.nonconverged = numnonconverged)))
    n <- object@n
    pmMCAR <- object@pmMCAR
    pmMAR <- object@pmMAR
    paramValue <- object@paramValue
    misspecValue <- object@misspecValue
    popFit <- object@popFit
    if (length(unique(n)) > 1) {
        temp1 <- n[converged]
        temp2 <- n[!converged]
        resultTemp <- c(mean.convergence = mean(temp1), sd.convergence = sd(temp1), 
            mean.nonconvergence = mean(temp2), sd.nonconvergence = sd(temp2), diff.mean = mean(temp1) - 
                mean(temp2), diff.sd = sd(temp1) - sd(temp2))
        result <- c(result, list(n = resultTemp))
    }
    if (length(unique(pmMCAR)) > 1) {
        temp1 <- pmMCAR[converged]
        temp2 <- pmMCAR[!converged]
        resultTemp <- c(mean.convergence = mean(temp1), sd.convergence = sd(temp1), 
            mean.nonconvergence = mean(temp2), sd.nonconvergence = sd(temp2), diff.mean = mean(temp1) - 
                mean(temp2), diff.sd = sd(temp1) - sd(temp2))
        result <- c(result, list(pmMCAR = resultTemp))
    }
    if (length(unique(pmMAR)) > 1) {
        temp1 <- pmMAR[converged]
        temp2 <- pmMAR[!converged]
        resultTemp <- c(mean.convergence = mean(temp1), sd.convergence = sd(temp1), 
            mean.nonconvergence = mean(temp2), sd.nonconvergence = sd(temp2), diff.mean = mean(temp1) - 
                mean(temp2), diff.sd = sd(temp1) - sd(temp2))
        result <- c(result, list(pmMAR = resultTemp))
    }
    temp1 <- paramValue[converged, ]
    temp2 <- paramValue[!converged, ]
    resultTemp <- cbind(mean.convergence = apply(temp1, 2, mean), sd.convergence = apply(temp1, 
        2, sd), mean.nonconvergence = apply(temp2, 2, mean), sd.nonconvergence = apply(temp2, 
        2, sd))
    resultTemp <- cbind(resultTemp, diff.mean = resultTemp[, 1] - resultTemp[, 3], 
        diff.sd = resultTemp[, 2] - resultTemp[, 4])
    result <- c(result, list(paramValue = resultTemp))
    if (!all(dim(misspecValue) == 0)) {
        temp1 <- misspecValue[converged, ]
        temp2 <- misspecValue[!converged, ]
        resultTemp <- cbind(mean.convergence = apply(temp1, 2, mean), sd.convergence = apply(temp1, 
            2, sd), mean.nonconvergence = apply(temp2, 2, mean), sd.nonconvergence = apply(temp2, 
            2, sd))
        resultTemp <- cbind(resultTemp, diff.mean = resultTemp[, 1] - resultTemp[, 
            3], diff.sd = resultTemp[, 2] - resultTemp[, 4])
        result <- c(result, list(misspecValue = resultTemp))
    }
    if (!all(dim(popFit) == 0)) {
        temp1 <- popFit[converged, ]
        temp2 <- popFit[!converged, ]
        resultTemp <- cbind(mean.convergence = apply(temp1, 2, mean), sd.convergence = apply(temp1, 
            2, sd), mean.nonconvergence = apply(temp2, 2, mean), sd.nonconvergence = apply(temp2, 
            2, sd))
        resultTemp <- cbind(resultTemp, diff.mean = resultTemp[, 1] - resultTemp[, 
            3], diff.sd = resultTemp[, 2] - resultTemp[, 4])
        result <- c(result, list(popFit = resultTemp))
    }
    return(result)
} 

# setPopulation: Set population parameter values

setPopulation <- function(target, population) {
    psl <- generate(population, n = 20, params = TRUE)$psl
    paramSet <- lapply(psl, "[[", 1)
    indLabGen <- NULL
    if (population@modelType == "Path") {
        indLabGen <- unique(population@pt$lhs)
    } else {
        indLabGen <- unique(population@pt$rhs[population@pt$op == "=~"])
    }
    facLabGen <- NULL
    if (population@modelType != "Path") {
        facLabGen <- unique(population@pt$lhs[population@pt$op == "=~"])
    }
    popParam <- reduceParamSet(paramSet, population@dgen, indLabGen, facLabGen)
    target@paramValue <- as.data.frame(t(data.frame(param = popParam)))
    return(target)
} 

# getPopulation: Description: Extract the population value from an object

getPopulation <- function(object) {
    return(object@paramValue)
} 

# getExtraOutput: Extract the extra output that users set in the 'outfun' argument

getExtraOutput <- function(object) {
    if (length(object@extraOut) == 0) {
        stop("This simulation result does not contain any extra results")
    } else {
        return(object@extraOut[object@converged])
    }
} 
