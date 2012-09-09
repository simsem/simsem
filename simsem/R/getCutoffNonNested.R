# getCutoffNonNested: get the cutoff from the simulated sampling distribution
# of difference in fit indices

# Compute fit1 - fit2
getCutoffNonNested <- function(dat1Mod1, dat1Mod2, dat2Mod1 = NULL, dat2Mod2 = NULL, 
    alpha = 0.05, usedFit = NULL, onetailed = FALSE, nVal = NULL, pmMCARval = NULL, 
    pmMARval = NULL, df = 0) {
    if (is.null(usedFit)) 
        usedFit <- getKeywords()$usedFit
    mod1 <- clean(dat1Mod1, dat1Mod2)
    dat1Mod1 <- mod1[[1]]
    dat1Mod2 <- mod1[[2]]
    if (!isTRUE(all.equal(unique(dat1Mod1@paramValue), unique(dat1Mod2@paramValue)))) 
        stop("'dat1Mod1' and 'dat1Mod2' are based on different data and cannot be compared, check your random seed")
    if (!is.null(dat2Mod1) & !is.null(dat2Mod2)) {
        mod2 <- clean(dat2Mod1, dat2Mod2)
        dat2Mod1 <- mod2[[1]]
        dat2Mod2 <- mod2[[2]]
        if (!isTRUE(all.equal(unique(dat2Mod1@paramValue), unique(dat2Mod2@paramValue)))) 
            stop("'dat2Mod1' and 'dat2Mod2' are based on different data and cannot be compared, check your random seed")
        if (!multipleAllEqual(unique(dat1Mod1@n), unique(dat1Mod2@n), unique(dat2Mod1@n), 
            unique(dat2Mod2@n))) 
            stop("Models are based on different values of sample sizes")
        if (!multipleAllEqual(unique(dat1Mod1@pmMCAR), unique(dat1Mod2@pmMCAR), unique(dat2Mod1@pmMCAR), 
            unique(dat2Mod2@pmMCAR))) 
            stop("Models are based on different values of the percent completely missing at random")
        if (!multipleAllEqual(unique(dat1Mod1@pmMAR), unique(dat1Mod2@pmMAR), unique(dat2Mod1@pmMAR), 
            unique(dat2Mod2@pmMAR))) 
            stop("Models are based on different values of the percent missing at random")
    } else {
        if (!isTRUE(all.equal(unique(dat1Mod1@n), unique(dat1Mod2@n)))) 
            stop("Models are based on different values of sample sizes")
        if (!isTRUE(all.equal(unique(dat1Mod1@pmMCAR), unique(dat1Mod2@pmMCAR)))) 
            stop("Models are based on different values of the percent completely missing at random")
        if (!isTRUE(all.equal(unique(dat1Mod1@pmMAR), unique(dat1Mod2@pmMAR)))) 
            stop("Models are based on different values of the percent missing at random")
    }
    if (is.null(nVal) || is.na(nVal)) 
        nVal <- NULL
    if (is.null(pmMCARval) || is.na(pmMCARval)) 
        pmMCARval <- NULL
    if (is.null(pmMARval) || is.na(pmMARval)) 
        pmMARval <- NULL
    Data1 <- as.data.frame((dat1Mod1@fit - dat1Mod2@fit))
    Data2 <- NULL
    if (!is.null(dat2Mod1) & !is.null(dat2Mod2)) 
        Data2 <- as.data.frame((dat2Mod1@fit - dat2Mod2@fit))
    
    condition <- c(length(unique(dat1Mod1@pmMCAR)) > 1, length(unique(dat1Mod1@pmMAR)) > 
        1, length(unique(dat1Mod1@n)) > 1)
    condValue <- cbind(dat1Mod1@pmMCAR, dat1Mod1@pmMAR, dat1Mod1@n)
    colnames(condValue) <- c("Percent MCAR", "Percent MAR", "N")
    condValue <- condValue[, condition]
    if (is.null(condValue) || length(condValue) == 0) 
        condValue <- NULL
    predictorVal <- rep(NA, 3)
    if (condition[3]) {
        ifelse(is.null(nVal), stop("Please specify the sample size value, 'nVal', because the sample size in the result object is varying"), 
            predictorVal[3] <- nVal)
    }
    if (condition[1]) {
        ifelse(is.null(pmMCARval), stop("Please specify the percent of completely missing at random, 'pmMCARval', because the percent of completely missing at random in the result object is varying"), 
            predictorVal[1] <- pmMCARval)
    }
    if (condition[2]) {
        ifelse(is.null(pmMARval), stop("Please specify the percent of missing at random, 'pmMARval', because the percent of missing at random in the result object is varying"), 
            predictorVal[2] <- pmMARval)
    }
    predictorVal <- predictorVal[condition]
    result <- list()
    if (onetailed) {
        cutoffDat1 <- getCutoff(Data1, alpha, FALSE, usedFit, predictor = condValue, 
            predictorVal = predictorVal, df = df)
        bound <- rep(-Inf, length(cutoffDat1))
        bound[names(cutoffDat1) %in% c("TLI", "CFI")] <- Inf
        resultModel1 <- rbind(bound, cutoffDat1)
        resultModel1 <- apply(resultModel1, 2, sort)
        rownames(resultModel1) <- c("lower", "upper")
        result$model1 <- resultModel1
        if (!is.null(dat2Mod1) & !is.null(dat2Mod2)) {
            cutoffDat2 <- getCutoff(Data2, 1 - alpha, FALSE, usedFit, predictor = condValue, 
                predictorVal = predictorVal, df = df)
            bound <- rep(Inf, length(cutoffDat2))
            bound[names(cutoffDat2) %in% c("TLI", "CFI")] <- -Inf
            resultModel2 <- rbind(bound, cutoffDat2)
            resultModel2 <- apply(resultModel2, 2, sort)
            rownames(resultModel2) <- c("lower", "upper")
            result$model2 <- resultModel2
        }
    } else {
        lower <- alpha/2
        upper <- 1 - (alpha/2)
        cutoffDat1Low <- getCutoff(Data1, lower, FALSE, usedFit, predictor = condValue, 
            predictorVal = predictorVal, df = df)
        cutoffDat1High <- getCutoff(Data1, upper, FALSE, usedFit, predictor = condValue, 
            predictorVal = predictorVal, df = df)
        resultModel1 <- rbind(cutoffDat1Low, cutoffDat1High)
        resultModel1 <- apply(resultModel1, 2, sort)
        rownames(resultModel1) <- c("lower", "upper")
        result$model1 <- resultModel1
        if (!is.null(dat2Mod1) & !is.null(dat2Mod2)) {
            cutoffDat2Low <- getCutoff(Data2, lower, FALSE, usedFit, predictor = condValue, 
                predictorVal = predictorVal, df = df)
            cutoffDat2High <- getCutoff(Data2, upper, FALSE, usedFit, predictor = condValue, 
                predictorVal = predictorVal, df = df)
            resultModel2 <- rbind(cutoffDat2Low, cutoffDat2High)
            resultModel2 <- apply(resultModel2, 2, sort)
            rownames(resultModel2) <- c("lower", "upper")
            result$model2 <- resultModel2
        }
    }
    return(result)
} 
