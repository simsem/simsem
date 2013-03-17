
getCIwidth <- function(object, assurance = 0.50, nVal = NULL, pmMCARval = NULL, pmMARval = NULL, df = 0) {
    if (is.null(nVal) || is.na(nVal)) 
        nVal <- NULL
    if (is.null(pmMCARval) || is.na(pmMCARval)) 
        pmMCARval <- NULL
    if (is.null(pmMARval) || is.na(pmMARval)) 
        pmMARval <- NULL
    object <- clean(object)
    Data <- as.data.frame(object@ciupper - object@cilower)
    condValuePredictorVal <- getCondValuePredictorVal(object, nVal, pmMCARval, pmMARval)
    
    output <- getCutoffDataFrame(Data, alpha = 1 - assurance, revDirec = FALSE, usedFit = colnames(Data), predictor = condValuePredictorVal[[1]], predictorVal = condValuePredictorVal[[2]], df = df)
	names(output) <- colnames(Data)
    return(output)
}
