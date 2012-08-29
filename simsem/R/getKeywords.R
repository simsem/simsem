# getKeywords: List of all keywords used in the simsem package

getKeywords <- function() {
    
    usedFit <- c("Chi", "AIC", "BIC", "RMSEA", "CFI", "TLI", "SRMR")
    usedFitPop <- c("f0", "rmsea", "srmr")
    optMin <- c("min", "minimum", "lower")
    optMax <- c("max", "maximum", "upper")
    optNone <- c("none", "null")
    
    result <- list(usedFit = usedFit, 
        usedFitPop = usedFitPop, optMin = optMin, optMax = optMax, optNone = optNone)
    return(result)
} 
