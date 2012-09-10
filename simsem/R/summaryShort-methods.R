# summaryShort: Provide short summary if it is available. Otherwise, it is an
# alias for summary.

setMethod("summaryShort", signature = "ANY", definition = function(object) {
    summary(object)
})

setMethod("summaryShort", signature = "SimMatrix", definition = function(object) {
    Data <- object@free
    Labels <- object@popParam
	if(!(all(dim(Labels) == 1) && is.nan(Labels))) {
		Labels[!is.free(Data)] <- as.character(Data[!is.free(Data)])
		Labels[is.free(Data)] <- paste(Data[is.free(Data)], Labels[is.free(Data)], sep = ":")
	} else {
		Labels <- Data
	}
	Mis <- object@misspec
    if (!(all(dim(Mis) == 1) && is.nan(Mis))) {
        Labels <- matrix(paste0(Labels, "+", Mis), nrow(Labels), ncol(Labels))
    }
    print(Labels)
})

setMethod("summaryShort", signature = "SimVector", definition = function(object) {
    Data <- object@free
    Labels <- object@popParam
	if(!(length(Labels) == 1 && is.nan(Labels))) {
		Labels[!is.free(Data)] <- as.character(Data[!is.free(Data)])
		Labels[is.free(Data)] <- paste(Data[is.free(Data)], Labels[is.free(Data)], sep = ":")
	} else {
		Labels <- Data
	}
    Mis <- object@misspec
    if (length(Mis) != 0 && !(length(Mis) == 1 && is.nan(Mis))) {
        Labels <- paste0(Labels, "+", Mis)
    }
    print(Labels)
}) 

setMethod("summaryShort", signature = "SimResult", definition = function(object, alpha=0.05, digits=3) {
    cat("RESULT OBJECT\n")
    cat(paste("Model Type:", print(object@modelType),"\n"))
    cleanObj <- clean(object)
	cat(paste("Convergence", sum(object@converged == TRUE), "/", object@nRep, "\n"))
	if (length(unique(object@n)) > 1) {
        cat(paste("Sample size:", min(object@n), "to", max(object@n),"\n"))
	} else {
		cat(paste("Sample size:", unique(object@n),"\n"))
	}
	if (length(unique(object@pmMCAR)) > 1) {
        cat(paste("Sample size:", min(object@pmMCAR), "to", max(object@pmMCAR),"\n"))
	} else {
		cat(paste("Sample size:", unique(object@pmMCAR),"\n"))
	}
	if (length(unique(object@pmMAR)) > 1) {
        cat(paste("Sample size:", min(object@pmMAR), "to", max(object@pmMAR),"\n"))
	} else {
		cat(paste("Sample size:", unique(object@pmMAR),"\n"))
	}
    cat("========= Fit Indices Cutoffs ============\n")
    print(summaryFit(object, alpha = alpha), digits)
    if (!is.null(object@paramValue)) {
        if ((ncol(object@coef) != ncol(object@paramValue)) | ((ncol(object@coef) == 
            ncol(object@paramValue)) && any(sort(colnames(object@coef)) != sort(colnames(object@paramValue))))) 
            cat("NOTE: The data generation model is not the same as the analysis model. See the summary of the population underlying data generation by the summaryPopulation function.\n")
    }
}) 
