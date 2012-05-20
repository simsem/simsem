# summaryParam: This function will summarize the obtained parameter estimates and standard error.

setMethod("summaryParam", signature(object = "SimResult"), definition = function(object, alpha = 0.05, detail = FALSE) {
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
    
    colnames(result) <- c("Estimate Average", "Estimate SD", "Average SE", "Power (Not equal 0)", "Std Est", "Std Est SD")
    if (!isNullObject(object@paramValue) && (ncol(object@coef) == ncol(object@paramValue)) && all(colnames(object@coef) == colnames(object@paramValue))) {
        nRep <- nrow(object@coef)
        nParam <- ncol(object@coef)
        paramValue <- object@paramValue
        if (nrow(object@paramValue) == 1) 
            paramValue <- matrix(unlist(rep(paramValue, nRep)), nRep, nParam, byrow = T)
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
        colnames(result2) <- c("Average Param", "SD Param", "Average Bias", "SD Bias", "Coverage")
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
    if (!isNullObject(object@FMI1) & !isNullObject(object@FMI2)) {
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
        result <- data.frame(result, resultFMI)
    }
    if (length(unique(object@n)) > 1) {
        corCoefN <- cor(cbind(object@coef, object@n), use = "pairwise.complete.obs")[colnames(object@coef), "object@n"]
        corSeN <- cor(cbind(object@se, object@n), use = "pairwise.complete.obs")[colnames(object@se), "object@n"]
        result <- data.frame(result, r_coef.n = corCoefN, r_se.n = corSeN)
    }
    if (length(unique(object@pmMCAR)) > 1) {
        corCoefMCAR <- cor(cbind(object@coef, object@pmMCAR), use = "pairwise.complete.obs")[colnames(object@coef), "object@pmMCAR"]
        corSeMCAR <- cor(cbind(object@se, object@pmMCAR), use = "pairwise.complete.obs")[colnames(object@se), "object@pmMCAR"]
        result <- data.frame(result, r_coef.pmMCAR = corCoefMCAR, r_se.pmMCAR = corSeMCAR)
    }
    if (length(unique(object@pmMAR)) > 1) {
        corCoefMAR <- cor(cbind(object@coef, object@pmMAR), use = "pairwise.complete.obs")[colnames(object@coef), "object@pmMAR"]
        corSeMAR <- cor(cbind(object@se, object@pmMAR), use = "pairwise.complete.obs")[colnames(object@se), "object@pmMAR"]
        result <- data.frame(result, r_coef.pmMAR = corCoefMAR, r_se.pmMAR = corSeMAR)
    }
    return(as.data.frame(result))
})

setMethod("summaryParam", signature(object = "SimModelOut"), definition = function(object, alpha = 0.05) {
    lab <- makeLabels(object@param, "OpenMx")
    coef <- vectorizeObject(object@coef, lab)
    se <- vectorizeObject(object@se, lab)
    se[se == 0] <- NA
    z <- coef/se
    p <- (1 - pnorm(abs(z))) * 2
    stdSet <- standardize(object)
    std <- vectorizeObject(stdSet, lab)
    result <- cbind(coef, se, z, p, std)
    colnames(result) <- c("Estimate", "SE", "z", "p", "Std Est")
    if (!isNullObject(object@paramValue)) {
        paramValue <- vectorizeObject(object@paramValue, lab)
        biasParam <- vectorizeObject(subtractObject(object@coef, object@paramValue), lab)
        crit <- qnorm(1 - alpha/2)
        lowerBound <- coef - crit * se
        upperBound <- coef + crit * se
        cover <- (paramValue > lowerBound) & (paramValue < upperBound)
        result <- data.frame(result, Param = paramValue, Bias = biasParam, Coverage = cover)
    }
    return(as.data.frame(result))
})

setMethod("summaryParam", signature(object = "SimModelMIOut"), definition = function(object, alpha = 0.05) {
    lab <- makeLabels(object@param, "OpenMx")
    coef <- vectorizeObject(object@coef, lab)
    se <- vectorizeObject(object@se, lab)
    stdSet <- standardize(object)
    std <- vectorizeObject(stdSet, lab)
    FMI1 <- vectorizeObject(object@FMI1, lab)
    FMI2 <- vectorizeObject(object@FMI2, lab)
    se[se == 0] <- NA
    z <- coef/se
    p <- (1 - pnorm(abs(z))) * 2
    result <- cbind(coef, se, z, p, std, FMI1, FMI2)
    colnames(result) <- c("Estimate", "SE", "z", "p", "Std Est", "FMI1", "FMI2")
    if (!isNullObject(object@paramValue)) {
        paramValue <- vectorizeObject(object@paramValue, lab)
        biasParam <- vectorizeObject(subtractObject(object@coef, object@paramValue), lab)
        crit <- qnorm(1 - alpha/2)
        lowerBound <- coef - crit * se
        upperBound <- coef + crit * se
        cover <- (paramValue > lowerBound) & (paramValue < upperBound)
        result <- data.frame(result, Param = paramValue, Bias = biasParam, Coverage = cover)
    }
    return(as.data.frame(result))
})

setMethod("summaryParam", signature(object = "SimResultParam"), definition = function(object) {
    average <- colMeans(object@param, na.rm = TRUE)
    se <- sapply(object@param, sd, na.rm = TRUE)
    result <- data.frame(mean = average, sd = se)
    return(result)
}) 
