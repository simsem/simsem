# getCutoffNested: get the cutoff from the simulated sampling distribution of
# difference in fit indices

# model1: nested model --> more df model2: parent model --> less df
getCutoffNested <- function(nested, parent, alpha = 0.05, usedFit = NULL, nVal = NULL, 
    pmMCARval = NULL, pmMARval = NULL, df = 0) {
    mod <- clean(nested, parent)
    nested <- mod[[1]]
    parent <- mod[[2]]
    if (!isTRUE(all.equal(unique(nested@paramValue), unique(parent@paramValue)))) 
        stop("Models are based on different data and cannot be compared, check your random seed")
    if (!isTRUE(all.equal(unique(nested@n), unique(parent@n)))) 
        stop("Models are based on different values of sample sizes")
    if (!isTRUE(all.equal(unique(nested@pmMCAR), unique(parent@pmMCAR)))) 
        stop("Models are based on different values of the percent completely missing at random")
    if (!isTRUE(all.equal(unique(nested@pmMAR), unique(parent@pmMAR)))) 
        stop("Models are based on different values of the percent missing at random")
    if (is.null(nVal) || is.na(nVal)) 
        nVal <- NULL
    if (is.null(pmMCARval) || is.na(pmMCARval)) 
        pmMCARval <- NULL
    if (is.null(pmMARval) || is.na(pmMARval)) 
        pmMARval <- NULL
    Data <- as.data.frame((nested@fit - parent@fit))
    condition <- c(length(unique(nested@pmMCAR)) > 1, length(unique(nested@pmMAR)) > 
        1, length(unique(nested@n)) > 1)
    condValue <- cbind(nested@pmMCAR, nested@pmMAR, nested@n)
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
    output <- getCutoffDataFrame(Data, alpha, FALSE, usedFit, predictor = condValue, predictorVal = predictorVal, df = df)
    return(output)
} 
