# summaryShort: Provide short summary if it is available. Otherwise, it is an alias for summary.

setMethod("summaryShort", signature = "ANY", definition = function(object) {
    summary(object)
})

setMethod("summaryShort", signature = "SimMatrix", definition = function(object) {
    Data <- object@free
    Labels <- object@popParam
    Labels[!is.na(Data)] <- as.character(Data[!is.na(Data)])
    Labels[is.na(Data)] <- paste("NA:", Labels[is.na(Data)], sep = "")
	Mis <- object@misspec
	if(!(all(dim(Mis) == 1) && is.nan(Mis))) {
		Labels <- matrix(paste(Labels, Mis, sep=":"), nrow(Labels), ncol(Labels))
	}
    print(Labels)
})

setMethod("summaryShort", signature = "SimVector", definition = function(object) {
    Data <- object@free
    Labels <- object@popParam
    Labels[!is.na(Data)] <- as.character(Data[!is.na(Data)])
    Labels[is.na(Data)] <- paste("NA:", Labels[is.na(Data)], sep = "")
	Mis <- object@misspec
	if(!(length(Mis) == 1 && is.nan(Mis))) {
		Labels <- paste(Labels, Mis, sep=":")
	}
    print(Labels)
})
